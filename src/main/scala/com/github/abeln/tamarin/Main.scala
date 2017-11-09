package com.github.abeln.tamarin

import com.github.abeln.tamarin.SymInstr._

/**
  * Entry point to Tamarin
  */
object Main {

  def main(args: Array[String]): Unit = {
    val proc = Desugar andThen SSAConv

    println(Query.solve(proc(trace(
      Mult(Reg(1), Reg(2)),
      Mfhi(Reg(3)),
      EqCond(Reg(3), Lit(25))
    ))))
  }
}
