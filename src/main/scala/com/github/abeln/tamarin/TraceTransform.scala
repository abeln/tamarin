package com.github.abeln.tamarin
import com.github.abeln.tamarin.Err.err
import com.github.abeln.tamarin.SymInstr._

/**
  * Transformation pipeline for traces.
  */
object TraceTransform {

  private implicit def instrToTrace(i: Instr): Trace = Seq(i)

  /**
    * Desugars "complicated" MIPS instructions.
    */
  private def desugar(tr: Trace): Trace = {
    tr.flatMap {
      case Jalr(concretePC) => assign(savedPC, Lit(concretePC))
      case Mult(s, t) => trace(
        Mult64(TMP, s, t),
        Low32(LO, TMP),
        High32(HI, TMP)
      )
      case MultU(s, t) => trace(
        MultU64(TMP, s, t),
        Low32(LO, TMP),
        High32(HI, TMP)
      )
      case Div(s, t) => trace(
        Quot(LO, s, t),
        Rem(HI, s, t)
      )
      case DivU(s, t) => trace(
        QuotU(LO, s, t),
        RemU(HI, s, t)
      )
      case Mflo(d) => assign(d, LO)
      case Mfhi(d) => assign(d, HI)
      case instr => instr
    }
  }

  /**
    * Simplifies traces by removing trivial constraints.
    */
  private def simplify(trace: Trace): Trace = {
    trace filter {
        case EqCond(r1, r2) if r1 == r2 => false
        case NeqCond(r1, r2) if r1 == r2 => false
        case _ => true
    }
  }

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
  def convertToSSA(trace: Trace): Trace = {
    /** The set of modifiable registers */
    val mods: Set[Reg] = addressable + LO + HI

    /** Records a mutation of register `r`. Returns the new copy of the register. */
    def update(r: Reg)(implicit last: Map[Reg, Reg]): Map[Reg, Reg] = {
      require(mods.contains(r), s"Register ${r.r} isn't modifiable")
      last + (r -> freshRegister())
    }

    /** Returns the copy of `o` that was last-modified. */
    def lookup(o: Operand)(implicit last: Map[Reg, Reg]): Operand = o match {
      case r: Reg => last(r)
      case _ => o
    }

    // Helpers for generic update of source and destination registers
    def reapply3(f: (Reg, Operand, Operand) => Instr, d: Reg, s: Operand, t: Operand)(implicit last: Map[Reg, Reg]): (Map[Reg, Reg], Instr) = {
      // Make sure to look up the read operands *before* updating the destination.
      val o1 = lookup(s)
      val o2 = lookup(t)
      val newLast = update(d)
      (newLast, f(newLast(d), o1, o2))
    }

    def reapply2(f: (Reg, Reg) => Instr, d: Reg, s: Reg)(implicit last: Map[Reg, Reg]): (Map[Reg, Reg], Instr) = {
      // Look-up operand before updating.
      val o1 = lookup(s)
      val newLast = update(d)
      (newLast, f(newLast(d), o1.asInstanceOf[Reg]))
    }

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

  /** Trims a trace so that it contains at most `depth` path conditions. */
  private def trim(trace: Trace)(implicit depth: Int): Trace = {
    var pc = 0
    trace.takeWhile { instr =>
      if (pc > depth) false
      else {
        if (isPC(instr)) pc += 1
        true
      }
    }
  }

  /**
    * Takes a trace as returned by the CPU, and transforms it so it can be consumed by the SMT solver.
    */
  def go(trace: Trace)(implicit depth: Int): Trace = {
    // These are all composed in-order.
    val phases = Seq[Trace => Trace](
      desugar, // this should go first, since the other phases assume they won't see some of the eliminated forms
      simplify,
      trim, // should go after simplify, so we don't trim trivial path conditions
      convertToSSA
    )

    (trace /: phases) {
      case (currTrace, phase) => phase(currTrace)
    }
  }
}
