package com.github.abeln.tamarin

import com.github.abeln.tamarin.SymInstr._

/**
  * Entry point to Tamarin
  */
object Main {

  def main(args: Array[String]): Unit = {
    val proc = Desugar andThen SSAConv

    println(Query.solve(proc(trace(
      Add(Reg(4), Lit(42), Lit(0)),
      Sw(Reg(4), 0, Lit(100)),
      Lw(Reg(3), 50, Reg(1)),
      EqCond(Reg(3), Lit(42))
    ))))
  }
}
