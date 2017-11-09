package com.github.abeln.tamarin.mips

/** A representation of the state of the MIPS CPU and memory. */
abstract class State {
  /** Read the value of register `number`. */
  def reg(number: Long): Word
  /** Read the value at memory address `address`. */
  def mem(address: Word): Word
  /** Set the value of register `number` to `word`. */
  def setReg(number: Long, word: Word): State
  /** Set the value at memory address `address` to `word`. */
  def setMem(address: Word, word: Word): State
}

object State {
  /** The special program counter register. */
  val PC = 32L
  /** The special register holding the low-order 32 bits of a multiplication or the quotient of a division. */
  val LO = 33L
  /** The special register holding the high-order 32 bits of a multiplication or the remainder of a division. */
  val HI = 34L
  /** Returns the initial state of the CPU. */
  def apply(): State = implementation.State().setReg(31, CPU.terminationPC).setReg(30, CPU.maxAddr)
}
