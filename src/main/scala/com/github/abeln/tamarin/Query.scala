package com.github.abeln.tamarin

import com.github.abeln.tamarin.SymInstr._

import collection.mutable
import com.microsoft.z3._

/**
  * Translates symbolic traces into Z3 queries.
  */
object Query extends Constraints {

  /** Values of the two input registers. Using longs because they can be unsigned 32-bit ints. */
  type Input = (Long, Long)

  private implicit var ctx = new Context()
  val bv32: Sort = ctx.mkBitVecSort(32)
  val bv64: Sort = ctx.mkBitVecSort(64)

  /** Maps a register to the expressions that represents it. */
  private var reg2exp = mutable.Map.empty[Reg, BitVecExpr]

  /** Queries the SAT solver to find out inputs that can satisfy the given trace. */
  def query(trace: Trace): Option[Input] = {
    ctx = new Context()
    val solver = ctx.mkSolver()
    reg2exp.clear()

    // Add initial constants.
    initRegs foreach { r =>
      reg2exp += (r -> declareConst32(r))
      if (!inputRegs.contains(r)) {
        // All registers are initialized to 0.
        solver.add(eq32(r, Lit(0)))
      }
    }

    // Process trace
    trace foreach {
      case Add(d, s, t) =>
        declareConst32(d)
        add32(d, s, t)
      case Sub(d, s, t) =>
        ???
    }

    if (solver.check() == Status.SATISFIABLE) {
      Some((42, 42))
    } else None
  }

  private def declareConst32(r: Reg): BitVecExpr = {
    val exp = ctx.mkConst(r.toString, bv32).asInstanceOf[BitVecExpr]
    reg2exp += (r -> exp)
    exp
  }


}
