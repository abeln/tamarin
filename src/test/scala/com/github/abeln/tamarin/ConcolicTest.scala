package com.github.abeln.tamarin

import com.github.abeln.tamarin.mips.{CPU, Word}
import com.github.abeln.tamarin.mips.code.ProgramRepresentation._
import org.scalatest.FlatSpec
import com.github.abeln.tamarin.mips.assembler.Assembler._
import com.github.abeln.tamarin.Concolic._
import com.github.abeln.tamarin.mips.assembler.Reg
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


  it should "conclude that a program is equal to itself" in {
    val onePlusTwo = Seq[Code](
      ADD(Reg(3), Reg(1), Reg(2)),
      JR(Reg(31))
    )

    assertMaybeEquiv(onePlusTwo, onePlusTwo)
  }

  it should "detect differences in small memory ops" in {
    val storeAndLoad = Seq[Code](
      const(Reg.scratch, 100),
      SW(Reg.input1, 0, Reg.scratch),
      LW(Reg.result, 0, Reg.scratch),
      JR(Reg.savedPC)
    )

    val buggyLabel = new Label("buggy")
    val storeAndLoadBuggy = Seq[Code](
      const(Reg.scratch, 100),
      SW(Reg.input1, 0, Reg.scratch),
      LW(Reg.result, 0, Reg.scratch),
      bne(Reg.result, Reg.scratch, buggyLabel),
      const(Reg.scratch, 42),
      ADD(Reg.result, Reg.result, Reg.scratch),
      Define(buggyLabel),
      JR(Reg.savedPC)
    )

    assertNotEquiv(storeAndLoad, storeAndLoadBuggy)
  }

  it should "deal with basic stack manipulations" in {
    val pushAndPop = Seq[Code](
      setUpStack,
      const(Reg.result, 42),
      push(Reg.result),
      push(Reg.input1),
      const(Reg.result, 200),
      push(Reg.result),
      pop(Reg.result),
      pop(Reg.result),
      JR(Reg.savedPC)
    )

    val skipBuggy = new Label("skipBuggy")

    val pushAndPopBuggy = Seq[Code](
      setUpStack,
      const(Reg.result, 42),
      push(Reg.result),
      push(Reg.input1),
      const(Reg.result, 200),
      push(Reg.result),
      pop(Reg.result),
      pop(Reg.result),
      const(Reg.scratch, 199),
      bne(Reg.result, Reg.scratch, skipBuggy),
      const(Reg.scratch, 100),
      ADD(Reg.result, Reg.result, Reg.scratch),
      Define(skipBuggy),
      JR(Reg.savedPC)
    )

    assertNotEquiv(pushAndPop, pushAndPopBuggy)
  }

  it should "handle function calls" in {
    // Correct impl
    val succLabel = new Label("succFn")
    val succFn = Seq[Code](
      Define(succLabel),
      push(Reg.savedPC),
      push(Reg.framePointer),
      ADD(Reg.framePointer, Reg.stackPointer),
      LW(Reg.result, 8, Reg.framePointer),
      const(Reg.scratch, 1),
      ADD(Reg.result, Reg.result, Reg.scratch),
      ADD(Reg.stackPointer, Reg.framePointer),
      pop(Reg.framePointer),
      pop(Reg.savedPC),
      JR(Reg.savedPC)
    )

    val caller = Seq[Code](
      setUpStack,
      push(Reg.savedPC),
      push(Reg.input1),
      LIS(Reg.targetPC),
      Use(succLabel),
      JALR(Reg.targetPC),
      pop(Reg.scratch),
      pop(Reg.savedPC),
      JR(Reg.savedPC)
    ) ++ succFn

    // Buggy impl
    val succBuggyLabel = new Label("succBuggyFn")
    val skipLabel = new Label("skip")

    val succBuggyFn = Seq[Code](
      Define(succBuggyLabel),
      push(Reg.savedPC),
      push(Reg.framePointer),
      ADD(Reg.framePointer, Reg.stackPointer),
      LW(Reg.result, 8, Reg.framePointer),
      const(Reg.scratch, 1),
      ADD(Reg.result, Reg.result, Reg.scratch),
      const(Reg.scratch, 107),
      bne(Reg.result, Reg.scratch, skipLabel),
      const(Reg.scratch, 1),
      ADD(Reg.result, Reg.result, Reg.scratch),
      Define(skipLabel),
      ADD(Reg.stackPointer, Reg.framePointer),
      pop(Reg.framePointer),
      pop(Reg.savedPC),
      JR(Reg.savedPC)
    )

    val callerBuggy = Seq[Code](
      setUpStack,
      push(Reg.savedPC),
      push(Reg.input1),
      LIS(Reg.targetPC),
      Use(succBuggyLabel),
      JALR(Reg.targetPC),
      pop(Reg.scratch),
      pop(Reg.savedPC),
      JR(Reg.savedPC)
    ) ++ succBuggyFn

    assertNotEquiv(caller, callerBuggy)
  }

  it should "handle loops" in {
    val startLoop = new Label("startLoop")
    val endLoop = new Label("endLoop")

    val prog1 = Seq[Code](
      const(Reg.scratch, 1),
      Define(startLoop),
      SLT(Reg.result, Reg.scratch, Reg.input1),
      beq(Reg.result, Reg.zero, endLoop),
      const(Reg.input2, 1),
      ADD(Reg.scratch, Reg.scratch, Reg.input2),
      beq(Reg.zero, Reg.zero, startLoop),
      Define(endLoop),
      const(Reg.input2, 45),
      SLT(Reg.result, Reg.scratch, Reg.input2),
      JR(Reg.savedPC)
    )

    val prog2 = Seq[Code](
      const(Reg.result, 1),
      JR(Reg.savedPC)
    )

    assertNotEquiv(prog1, prog2)
  }

  val setUpStack: Code = block(
    const(Reg.scratch, CPU.numMaxAddr - 4),
    ADD(Reg.stackPointer, Reg.scratch),
    ADD(Reg.framePointer, Reg.stackPointer)
  )

  def push(r: Reg): Code = {
    block(
      const(Reg.scratch, 4),
      SUB(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      SW(r, 0, Reg.stackPointer)
    )
  }

  def pop(r: Reg): Code = block(
    LW(r, 0, Reg.stackPointer),
    const(Reg.scratch, 4),
    ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch)
  )

  def const(r: Reg, v: Long): Code = {
    block(
      LIS(r),
      Word(encodeSigned(v))
    )
  }

  def assertNotEquiv(ref: Program, cand: Program): Unit = {
    val res = compare(ref, cand)
    res shouldBe a [NotEquiv]
    val NotEquiv(inps, refSol, candSol) = res
    refSol should not equal (candSol)
    val r1 = inps(0)
    val r2 = inps(1)
    val actualRef = runAndExpectSol(ref, r1, r2)
    val actualCand = runAndExpectSol(cand, r1, r2)
    actualRef should equal (refSol)
    actualCand should equal (candSol)
  }

  def runAndExpectSol(prog: Program, r1: Long, r2: Long): Long = {
    val CPU.Done(finalSt, _) = loadAndRun(
      Transformations.toMachineCode(prog),
      Word(encodeSigned(r1)),
      Word(encodeSigned(r2))
    ).asInstanceOf[CPU.Done]
    // TODO: don't hardcode the output register
    decodeSigned(finalSt.reg(3))
  }

  def assertMaybeEquiv(ref: Program, cand: Program): Unit = {
    compare(ref, cand) should equal (MaybeEquiv)
  }
}
