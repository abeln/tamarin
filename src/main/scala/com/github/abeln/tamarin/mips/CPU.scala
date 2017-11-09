package com.github.abeln.tamarin.mips

import State._

import scala.annotation.tailrec
import implementation._

/** Specification of the MIPS processor used to execute CS 241E programs. */

object CPU {
  /** Reads a register, enforcing that the value of register 0 is always zero. */
  def readRegister(state: State, regNum: Long) = if(regNum == 0) Word.zero else state.reg(regNum)

  /** Returns true if `address` is the valid address of some memory location. */
  def validAddress(address: Word): Boolean = {
    val addressAsNumber = asUnsigned(address)
    (addressAsNumber % 4) == 0 && addressAsNumber < asUnsigned(CPU.maxAddr)
  }

  /** Issues an error if `address` is not the valid address of any memory location. */
  def checkValidAddress(address: Word): Unit =
    if(!validAddress(address)) sys.error(s"attempt to dereference invalid address $address")

  /** Issues an error if `address` is not a valid value of the PC. */
  def checkValidPC(address: Word): Unit =
    if(!validAddress(address) && address != terminationPC) sys.error(s"attempt to assign invalid address $address to PC")

  /** Increments `address` by the number of bytes specified by `bytes`. */
  def incrementAddress(address: Word, bytes: Long = 4L): Word =
    encodeUnsigned(mod2to32(asUnsigned(address) + bytes))

  /** The transition function for one step of execution of the CPU. */
  def step(state0: State): State = {

    /* Fetch the instruction at the PC. */
    val pc = readRegister(state0, PC)
    val instruction = state0.mem(pc)

    /* Helper function to issue an invalid instruction error. */
    def invalidInstruction = sys.error(s"Invalid instruction $instruction at PC = $pc")

    /* Helper function to increment the PC by the number of words specified by `words`. */
    def incrementPC(state0: State, words: Long = 1): State = {
      val newPC = incrementAddress(readRegister(state0, PC), bytes = words*4)
      checkValidPC(newPC)
      state0.setReg(PC, newPC)
    }

    /* Increment the program counter. */
    val state1 = incrementPC(state0)

    /* Decode and execute the instruction. */
    val List(op, sBits, tBits, iBits) = instruction.splitAt(List(6, 5, 5))
    val s = implementation.asUnsigned(sBits)
    val t = implementation.asUnsigned(tBits)
    op match {
      case Bits("000000") =>
        val List(dBits, zeros, function) = iBits.splitAt(List(5, 5))
        val d = implementation.asUnsigned(dBits)
        if (zeros != Bits("00000")) invalidInstruction
        else function match {

          case Bits("100000") => // add
            val result = asUnsigned(readRegister(state1, s)) + asUnsigned(readRegister(state1, t))
            state1.setReg(d, encodeUnsigned(mod2to32(result)))

          case Bits("100010") => // sub
            val result = asUnsigned(readRegister(state1, s)) - asUnsigned(readRegister(state1, t))
            state1.setReg(d, encodeUnsigned(mod2to32(result)))

          case Bits("011000") if d == 0 => // mult
            val result = asSigned(readRegister(state1, s)) * asSigned(readRegister(state1, t))
            val (bitshi, bitslo) = encodeSigned64(result).splitAt(32)
            state1.setReg(LO, Word(bitslo)).setReg(HI, Word(bitshi))

          case Bits("011001") if d == 0 => // multu
            val result = asUnsigned(readRegister(state1, s)) * asUnsigned(readRegister(state1, t))
            val (bitshi, bitslo) = encodeUnsigned64(result).splitAt(32)
            state1.setReg(LO, Word(bitslo)).setReg(HI, Word(bitshi))

          case Bits("011010") if d == 0 => // div
            val quotient = asSigned(readRegister(state1, s)) / asSigned(readRegister(state1, t))
            val remainder = asSigned(readRegister(state1, s)) % asSigned(readRegister(state1, t))
            val bitslo = encodeSigned(mod2to32signed(quotient))
            val bitshi = encodeSigned(mod2to32signed(remainder))
            state1.setReg(LO, bitslo).setReg(HI, bitshi)

          case Bits("011011") if d == 0 => // divu
            val quotient = asUnsigned(readRegister(state1, s)) / asUnsigned(readRegister(state1, t))
            val remainder = asUnsigned(readRegister(state1, s)) % asUnsigned(readRegister(state1, t))
            val bitslo = encodeUnsigned(mod2to32(quotient))
            val bitshi = encodeUnsigned(mod2to32(remainder))
            state1.setReg(LO, bitslo).setReg(HI, bitshi)

          case Bits("010000") if s == 0 && t == 0 => // mfhi
            state1.setReg(d, readRegister(state1, HI))

          case Bits("010010") if s == 0 && t == 0 => // mflo
            state1.setReg(d, readRegister(state1, LO))

          case Bits("010100") if s == 0 && t == 0 => // lis
            val loadConstant = state1.setReg(d, state1.mem(readRegister(state1, PC)))
            incrementPC(loadConstant)

          case Bits("101010") => // slt
            val result = asSigned(readRegister(state1, s)) < asSigned(readRegister(state1, t))
            state1.setReg(d,  Word(Bits("0" * 31) :+ result))

          case Bits("101011") => // sltu
            val result = asUnsigned(readRegister(state1, s)) < asUnsigned(readRegister(state1, t))
            state1.setReg(d,  Word(Bits("0" * 31) :+ result))

          case Bits("001000") if t == 0 && d == 0 => // jr
            val newAddress = readRegister(state1, s)
            checkValidPC(newAddress)
            state1.setReg(PC, newAddress)

          case Bits("001001") if t == 0 && d == 0 => // jalr
            val newAddress = readRegister(state1, s)
            checkValidPC(newAddress)
            state1
              .setReg(31, readRegister(state1, PC))
              .setReg(PC, newAddress)

          case _ => invalidInstruction
        }

      case Bits("100011") => // lw
        val address = incrementAddress(readRegister(state1, s), asSigned(iBits))
        checkValidAddress(address)
        state1.setReg(t, state1.mem(address))

      case Bits("101011") => // sw
        val address = incrementAddress(readRegister(state1, s), asSigned(iBits))
        if(address == printAddr) {
          outputStream.print(implementation.asUnsigned(readRegister(state1, t).takeRight(8)).toChar)
          state1
        } else {
          checkValidAddress(address)
          state1.setMem(address, readRegister(state1, t))
        }

      case Bits("000100") => // beq
        if(readRegister(state1, s) == readRegister(state1, t))
          incrementPC(state1, words = implementation.asSigned(iBits))
        else state1

      case Bits("000101") => // bne
        if(readRegister(state1, s) != readRegister(state1, t))
          incrementPC(state1, words = implementation.asSigned(iBits))
        else state1

      case _ => invalidInstruction
    }
  }

  /** Run steps of the CPU starting in `state` until it reaches a state when the PC has the value `terminationPC`. */
  @tailrec def run(state: State): State = {
    if(state.reg(PC) == terminationPC) state
    else run(step(state))
  }

  /** The PC value at which the CPU should halt execution. */
  val terminationPC = Word("11111110111000011101111010101101")

  /** The address that is one word beyond the last valid memory address. */
  val maxAddr =       Word("00000001000000000000000000000000")

  /** The special address for producing output. When a word is stored to this address, the low-order 8 bits of
    * that word are written to standard output.
    */
  val printAddr =     Word("11111111111111110000000000001100")

  private[mips] var outputStream = System.out
}
