package com.github.abeln.tamarin

import com.github.abeln.tamarin.SymInstr._

/**
  * Entry point to Tamarin
  */
object Main {

  def main(args: Array[String]): Unit = {
    val proc = Desugar andThen SSAConv

    println(Query.solve(proc(trace(
      assign(Reg(2), Lit(2)),
      Div(Reg(1), Reg(2)),
      Mflo(Reg(3)),
      EqCond(Reg(3), Lit(4))
    ))))
  }
}
