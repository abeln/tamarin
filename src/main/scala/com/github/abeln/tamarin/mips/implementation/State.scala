package com.github.abeln.tamarin.mips.implementation

import com.github.abeln.tamarin.mips
import mips.State._
import mips.CPU

/** The implementation of a representation of the state of the MIPS CPU and memory. */
case class State(private[implementation] val memory: Memory, private[implementation] val regs: Vector[mips.Word]) extends mips.State {
  override def toString = {
    def regName(reg: Int) = reg match {
      case PC => "PC"
      case HI => "HI"
      case LO => "LO"
      case _ => "%02d".format(reg)
    }
    val strings = regs.toSeq.zipWithIndex.map { case (value, index) => s"${regName(index)}: $value" }
    strings.mkString("\n", "\n", "\n")
  }

  private def validAddr(word: mips.Word) = {
    val number = asUnsigned(word)
    (number & 3) == 0 && number < asUnsigned(CPU.maxAddr)
  }
  private def validReg(number: Long) = number >= 0 && number <= HI

  def reg(number: Long): mips.Word = {
    require(validReg(number))
    require(number > 0)
    regs(number.toInt)
  }
  def setReg(number: Long, word: mips.Word): mips.State = {
    require(validReg(number))
    if(number > 0) State(memory, regs.updated(number.toInt, word))
    else this
  }
  def mem(address: mips.Word): mips.Word = {
    require(validAddr(address))
    memory(asUnsigned(address).toInt)
  }
  def setMem(address: mips.Word, word: mips.Word): mips.State = {
    require(validAddr(address))
    State(memory.updated(asUnsigned(address).toInt, word), regs)
  }

  private[implementation] def setMem(addr0: mips.Word, words: TraversableOnce[mips.Word]): mips.State = {
    var addr = addr0
    def incrementAddress = encodeUnsigned(asUnsigned(addr) + 4)
    words.foldLeft[mips.State](this){case (state, word) =>
      val ret = state.setMem(addr, word)
      addr = incrementAddress
      ret
    }
  }
}

object State {
  /** Create a new initial state of the CPU, with register 31 set to `terminationPC`, register 30 set to `maxAddr`,
    * and all other registers and memory set to zero.
    */
  def apply(): mips.State =
    State(Memory(), Vector.fill(mips.State.HI.toInt+1){Word.zero})
    .setReg(31, CPU.terminationPC).setReg(30, CPU.maxAddr)
}
