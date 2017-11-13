package com.github.abeln.tamarin

import com.github.abeln.tamarin.mips.Word
import com.github.abeln.tamarin.mips.assembler.Assembler._
import com.github.abeln.tamarin.mips.assembler.Reg
import com.github.abeln.tamarin.mips.code.ProgramRepresentation._
import com.github.abeln.tamarin.mips.code.Transformations

/**
  * Entry point to Tamarin
  */
object Main {

  def main(args: Array[String]): Unit = {
    val proc = Desugar andThen SSAConv

//    println(Query.solve(proc(trace(
//      assign(Reg(2), Lit(2)),
//      Div(Reg(1), Reg(2)),
//      Mflo(Reg(3)),
//      EqCond(Reg(3), Lit(4))
//    ))))

    val beginLoop = new Label("beginLoop")
    val endLoop = new Label("endLoop")

    val onePlusTwo = Transformations.toMachineCode(Seq[Code](
      ADD(Reg.scratch, Reg(1), Reg(2)),
      LIS(Reg(5)),
      Word(encodeSigned(42)),
      Define(beginLoop),
      SLT(Reg(6), Reg.scratch, Reg(5)),
      beq(Reg(6), Reg.zero, endLoop),
      LIS(Reg(6)),
      Word(encodeSigned(1)),
      ADD(Reg.result, Reg.result, Reg(6)),
      ADD(Reg.scratch, Reg.scratch, Reg(6)),
      beq(Reg.zero, Reg.zero, beginLoop),
      Define(endLoop),
      JR(Reg(31))
    ))

    val (_, trace) = loadAndRun(onePlusTwo, Word(encodeSigned(20)), Word(encodeSigned(2)))
    println(trace)

    println(Query.solve(proc(trace)))
  }
}
