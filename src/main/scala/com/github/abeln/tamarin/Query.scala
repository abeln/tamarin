package com.github.abeln.tamarin


import com.github.abeln.tamarin.SymInstr._
import com.microsoft.z3._

/**
  * Interaction with the Z3 solver
  */
object Query {

  /** A solution is a sequence of values for the input registers. */
  type Soln = Seq[Long]

  /**
    * Determines whether the given `trace` can be satisfied by an assignment to
    * the input registers.
    * */
  def solve(trace: Trace): Option[Soln] = {
    implicit val ctx: Context = new Context
    val bv32 = ctx.mkBitVecSort(32)
    val bv64 = ctx.mkBitVecSort(64)
    val reg2consts = mkConsts(trace, bv32, bv64)
    val asserts = mkAsserts(trace, reg2consts, bv32, bv64)
    val solver = ctx.mkSolver()
    solver.add(asserts: _*)
    if (solver.check() == Status.SATISFIABLE) {
      val model = solver.getModel
      // TODO: find a better way to get the long value of a constant in the model
      Some(inputRegs.toSeq map (r => model.getConstInterp(reg2consts(r)).toString.toLong))
    } else None
  }

  /**
    * Extracts the registers that are used in the `trace`, together with a boolean indicating whether they're
    * 32-bit registers (false = 64 bits).
    * Assumes that no register is used without first being initialized.
    * */
  private def regsIn(trace: Trace): Set[(Reg, Boolean)] = {
    val init: Set[(Reg, Boolean)] = initRegs.map((_, true)).toSet
    init ++ trace.flatMap {
      // Since every register must first be initialized, we can get away with only handling
      // the mutating instructions.
      case Add(d, _, _) => Seq((d, true))
      case Sub(d, _, _) => Seq((d, true))
      case Lw(t, _, _) => Seq((t, true))
      case Slt(d, _, _) => Seq((d, true))
      case SltU(d, _, _) => Seq((d, true))
      case Mult64(d, _, _) => Seq((d, false))
      case MultU64(d, _, _) => Seq((d, false))
      case Low32(d, _) => Seq((d, true))
      case High32(d, _) => Seq((d, true))
      case Quot(d, _, _) => Seq((d, true))
      case QuotU(d, _, _) => Seq((d, true))
      case Rem(d, _, _) => Seq((d, true))
      case RemU(d, _, _) => Seq((d, true))
      case _ => Seq()
    }.toSet
  }

  // NOTE: all helpers that take in a `Context` potentially mutate it, as a side effect.

  /** Build a map mapping registers to Z3 constants in the context. */
  private def mkConsts(trace: Trace, bv32: Sort, bv64: Sort)(implicit ctx: Context): Map[Reg, BitVecExpr] = {
    (Map.empty[Reg, BitVecExpr] /: regsIn(trace)) {
      case (map, (reg, is32)) =>
        map + (reg -> ctx.mkConst(reg.toString, if (is32) bv32 else bv64).asInstanceOf[BitVecExpr])
    }
  }

  private def mkAsserts(trace: Trace, regMap: Map[Reg, BitVecExpr], bv32: Sort, bv64: Sort)(implicit ctx: Context): Seq[BoolExpr] = {
    def b32(o: Operand): BitVecExpr = o match {
      case Reg(r) => regMap(r)
      case Lit(v) => ctx.mkNumeral(v, bv32).asInstanceOf[BitVecExpr]
    }
    def b64(o: Operand): BitVecExpr = o match {
      case Reg(r) => regMap(r)
      case Lit(v) => ctx.mkNumeral(v, bv64).asInstanceOf[BitVecExpr]
    }
    (Seq.empty[BoolExpr] /: trace) { (asserts, instr) =>
      (instr match {
        case Add(d, s, t) =>
          Seq(ctx.mkEq(b32(d), ctx.mkBVAdd(b32(s), b32(t))))
        case Sub(d, s, t) =>
          Seq(ctx.mkEq(b32(d), ctx.mkBVSub(b32(s), b32(t))))
      }) ++ asserts
    }
  }
}
