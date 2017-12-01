package com.github.abeln.tamarin

import com.github.abeln.tamarin.mips.Word
import com.github.abeln.tamarin.mips.assembler.Assembler.{ADD, JR}
import com.github.abeln.tamarin.mips.assembler.Reg
import com.github.abeln.tamarin.mips.code.ProgramRepresentation._
import org.scalatest.FlatSpec
import com.github.abeln.tamarin.mips.assembler.Assembler._
import com.github.abeln.tamarin.Concolic._


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

    assert(compare(onePlusTwo, onePlusTwoWrong).isInstanceOf[NotEquiv])
  }
}
