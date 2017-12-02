package com.github.abeln.tamarin
import collection.mutable
import com.github.abeln.tamarin.SymInstr._
import com.github.abeln.tamarin.TraceMap.instrToTrace
import com.github.abeln.tamarin.Err.err

/**
  * Converts traces to SSA form.
  *
  * This is needed because the SMT solver can only reason about symbolic constants,
  * so repeated assignments to the same register need to be mapped to multiple
  * assignments to *different* (virtual) registers.
  *
  * Since we're only converting traces (no jumps), the conversion is straightforward.
  *
  * e.g.:
  *   add(r1, r2, r3)
  *   add(r1, r2, r4)
  *   sub(r4, r1, r2)
  *
  * becomes
  *   add(r1, r2, r3)
  *   add(r1', r2, r4)
  *   sub(r4', r1', r2)
  */
object SSAConv extends TraceMap {

  /** The set of modifiable registers */
  private val mods: Set[Reg] = addressable + LO + HI

  /** Records a mutation of register `r`. Returns the new copy of the register. */
  private def update(r: Reg)(implicit last: Map[Reg, Reg]): Map[Reg, Reg] = {
    require(mods.contains(r), s"Register ${r.r} isn't modifiable")
    last + (r -> freshRegister())
  }

  /** Returns the copy of `o` that was last-modified. */
  private def lookup(o: Operand)(implicit last: Map[Reg, Reg]): Operand = o match {
    case r: Reg => last(r)
    case _ => o
  }

  // Helpers for generic update of source and destination registers

  private def reapply3(f: (Reg, Operand, Operand) => Instr, d: Reg, s: Operand, t: Operand)(implicit last: Map[Reg, Reg]): (Map[Reg, Reg], Instr) = {
    // Make sure to look up the read operands *before* updating the destination.
    val o1 = lookup(s)
    val o2 = lookup(t)
    val newLast = update(d)
    (newLast, f(newLast(d), o1, o2))
  }

  private def reapply2(f: (Reg, Reg) => Instr, d: Reg, s: Reg)(implicit last: Map[Reg, Reg]): (Map[Reg, Reg], Instr) = {
    // Look-up operand before updating.
    val o1 = lookup(s)
    val newLast = update(d)
    (newLast, f(newLast(d), o1.asInstanceOf[Reg]))
  }

  override def apply(trace: Trace): Trace = {
    val modl = mods.toList
    val initLast = modl.zip(modl).toMap

    val (_, revTraces) = ((initLast, Seq.empty[Instr]) /: trace) {
      case ((lastMap, rTraces), instr) =>
        implicit val lastM = lastMap

        val (newLast, trace) = instr match {
          case Add(d, s, t) => reapply3(Add, d, s, t)
          case Sub(d, s, t) => reapply3(Sub, d, s, t)
          case Slt(d, s, t) => reapply3(Slt, d, s, t)
          case SltU(d, s, t) => reapply3(SltU, d, s, t)
          case Mult64(d, s, t) => reapply3(Mult64, d, s, t)
          case MultU64(d, s, t) => reapply3(MultU64, d, s, t)
          case Low32(d, s) => reapply2(Low32, d, s)
          case High32(d, s) => reapply2(High32, d, s)
          case Quot(d, s, t) => reapply3(Quot, d, s, t)
          case QuotU(d, s, t) => reapply3(QuotU, d, s, t)
          case Rem(d, s, t) => reapply3(Rem, d, s, t)
          case RemU(d, s, t) => reapply3(RemU, d, s, t)
          case Lw(t, o, s) =>
            val s2 = lookup(s)
            val newLast = update(t)
            (newLast, Lw(newLast(t), o, s2))
          case Sw(t, o, s) => (lastM, Sw(lookup(t).asInstanceOf[Reg], o, lookup(s)))
          case EqCond(s, t) => (lastM, EqCond(lookup(s).asInstanceOf[Reg], lookup(t)))
          case NeqCond(s, t) => (lastM, NeqCond(lookup(s).asInstanceOf[Reg], lookup(t)))
          case _ => err(s"Don't know how to SSA-convert $instr")
        }

        (newLast, trace +: rTraces)
    }

    revTraces.reverse.flatten
  }
}
