package com.github.abeln.tamarin

import com.github.abeln.tamarin.SymInstr._

/**
  * Entry point to Tamarin
  */
object Main {

  def main(args: Array[String]): Unit = {
    val proc = Desugar andThen SSAConv

    println(Query.solve(proc(trace(
      Add(Reg(3), Reg(1), Reg(2)),
      EqCond(Reg(3), Lit(42)),
      EqCond(Reg(2), Lit(100))
    ))))
  }
}
