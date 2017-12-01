package com.github.abeln.tamarin

import com.github.abeln.tamarin.mips.Word
import com.github.abeln.tamarin.mips.assembler.Assembler.{ADD, JR}
import com.github.abeln.tamarin.mips.assembler.{Assembler, Reg}
import com.github.abeln.tamarin.mips.code.ProgramRepresentation._
import org.scalatest.FlatSpec
import com.github.abeln.tamarin.mips.assembler.Assembler._
import com.github.abeln.tamarin.Concolic._
import org.scalatest.Matchers._
import com.github.abeln.tamarin.mips.code.Transformations


class ConcolicTest extends FlatSpec {

  "Concolic testing" should "find differences in a pair of simple programs" in {
    val onePlusTwo = Seq[Code](
      ADD(Reg(3), Reg(1), Reg(2)),
      JR(Reg(31))
    )

    val skip = new Label("skip")

    val onePlusTwoWrong = Seq[Code](
      ADD(Reg(3), Reg(1), Reg(2)),
      LIS(Reg(4)),
      Word(encodeSigned(42)),
      LIS(Reg(5)),
      Word(encodeSigned(103)),
      bne(Reg(1), Reg(4), skip),
      bne(Reg(2), Reg(5), skip),
      ADD(Reg(3), Reg(3), Reg(2)),
      Define(skip),
      JR(Reg(31))
    )

    assertNotEquiv(onePlusTwo, onePlusTwoWrong)
  }

  def assertNotEquiv(ref: Program, cand: Program): Unit = {
    val res = compare(ref, cand)
    res shouldBe a [NotEquiv]
    val NotEquiv(inps, refSol, candSol) = res
    refSol should not equal (candSol)
    val r1 = inps(0)
    val r2 = inps(1)
    val actualRef = runProg(ref, r1, r2)
    val actualCand = runProg(cand, r1, r2)
    actualRef should equal (refSol)
    actualCand should equal (candSol)
  }

  def runProg(prog: Program, r1: Long, r2: Long): Long = {
    val (finalSt, _) = Assembler.loadAndRun(
      Transformations.toMachineCode(prog),
      Word(Assembler.encodeSigned(r1)),
      Word(Assembler.encodeSigned(r2))
    )
    // TODO: don't hardcode the output register
    Assembler.decodeSigned(finalSt.reg(3))
  }

  def assertMaybeEquiv(ref: Program, cand: Program): Unit = {
    compare(ref, cand) should equal (MaybeEquiv)
  }
}
