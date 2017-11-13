package com.github.abeln.tamarin.mips.assembler

import com.github.abeln.tamarin.SymInstr.Trace
import com.github.abeln.tamarin.mips.{Bits, CPU, State, Word, twoTo}

import scala.collection.mutable

/** An assembler that generates machine language words representing MIPS instructions. */

object Assembler {

  /** Given a sequence of bits, interpret it as an unsigned binary number and return the number.
    *
    * Scala hint: Consult the Scala library documentation for classes such as Seq:
    * http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Seq
    *
    * The purpose of this assignment is for *you* to write code that encodes/decodes numbers in binary.
    * Do not submit solutions that just call functions in the Java/Scala standard library to do the
    * conversion for you.
    **/
  def decodeUnsigned(bits: Seq[Boolean]): Long = {
    if (bits.isEmpty) {
      0
    } else {
      var res = 0L
      var pow = 1L
      val rbits = bits.reverse
      for (i <- rbits.indices) {
        if (rbits(i)) {
          res += pow
        }
        pow *= 2
      }
      res
    }
  }

  /** Given a sequence of bits, interpret it as a signed two's-complement binary number and return the number. */
  def decodeSigned(bits: Seq[Boolean]): Long = {
    require(bits.nonEmpty)
    var res = decodeUnsigned(bits.drop(1))
    if (bits.head) {
      res += -twoTo(bits.length - 1)
    }
    res
  }

  /** Given a non-negative number `i`, encode it as an unsigned binary number using the number of bits
    * specified by `bits`.
    *
    * Scala hint: The `bits: Int = 32` specifies `32` as the default value of bits. When calling this method, one
    * can specify the number of bits explicitly (e.g. `encodeUnsigned(42, 8)`), or leave it unspecified
    * (e.g. `encodeUnsigned(42)`), in which case the default value of 32 will be used.
    *
    * The length of the output sequence must be equal to `bits`.
    **/
  def encodeUnsigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(i >= 0)
    require(i < twoTo(bits))
    val bin = mutable.ArrayBuffer.empty[Boolean]
    var ii = i
    for (j <- 0 until bits) {
      bin += (ii % 2 == 1)
      ii /= 2
    }
    bin.reverse
  }

  /** Given a number `i`, encode it as a signed two's-complement binary number using the number of bits
    * specified by `bits`.
    *
    * The length of the output sequence must be equal to `bits`.
    **/
  def encodeSigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(i >= -twoTo(bits-1))
    require(i < twoTo(bits-1))
    if (i >= 0) encodeUnsigned(i, bits)
    else Seq(true) ++ encodeUnsigned(i + twoTo(bits - 1), bits - 1)
  }

  /** If the given address is word-aligned, returns the address of the next word in memory.
    */
  def nextWord(addr: Word): Word = {
    val numAddr = decodeUnsigned(addr)
    require(numAddr % 4 == 0)
    Word(encodeUnsigned(numAddr + 4))
  }

  /* Each of the following methods should encode the corresponding MIPS machine language instruction as a 32-bit `Word`.
   *
   * Hint: One way to create a word is from a sequence of 32 Booleans.
   * One way to create a sequence of Booleans is using Bits.
   *
   * For example:
   * `val fourBits = Seq(true, false, true, false)`
   * `val moreBits = Bits("0101")`
   * `val eightBits = fourBits ++ moreBits`
   * `val word = Word(eightBits ++ eightBits ++ eightBits ++ eightBits)`
   */

  /* Hint: You may implement additional helper methods if you wish to factor out common code. */

  /** Produces the binary string (5 bits) corresponding to the given register's number:
    *   e.g. `binreg(4) == "00100"`
    */
  def binreg(r: Reg): String = {
    boolSeqToStr(encodeUnsigned(r.number, 5))
  }

  private def boolSeqToStr(digits: Seq[Boolean]): String = {
    (digits map {
      case true => '1'
      case false => '0'
    }).mkString
  }

  private def encodeImm(i: Long): String = {
    boolSeqToStr(encodeSigned(i, 16))
  }

  // Encodings copied from https://www.student.cs.uwaterloo.ca/~cs241e/current/mipsref.pdf

  def ADD(d: Reg, s: Reg, t: Reg = Reg.zero): Word = {
    Word(Bits(s"000000${binreg(s)}${binreg(t)}${binreg(d)}00000100000"))
  }

  def SUB(d: Reg, s: Reg, t: Reg): Word = {
    Word(Bits(s"000000${binreg(s)}${binreg(t)}${binreg(d)}00000100010"))
  }

  def MULT(s: Reg, t: Reg): Word = {
    Word(Bits(s"000000${binreg(s)}${binreg(t)}0000000000011000"))
  }

  def MULTU(s: Reg, t: Reg): Word = {
    Word(Bits(s"000000${binreg(s)}${binreg(t)}0000000000011001"))
  }

  def DIV(s: Reg, t: Reg): Word = {
    Word(Bits(s"000000${binreg(s)}${binreg(t)}0000000000011010"))
  }

  def DIVU(s: Reg, t: Reg): Word = {
    Word(Bits(s"000000${binreg(s)}${binreg(t)}0000000000011011"))
  }

  def MFHI(d: Reg): Word = {
    Word(Bits(s"0000000000000000${binreg(d)}00000010000"))
  }

  def MFLO(d: Reg): Word = {
    Word(Bits(s"0000000000000000${binreg(d)}00000010010"))
  }

  def LIS(d: Reg): Word = {
    Word(Bits(s"0000000000000000${binreg(d)}00000010100"))
  }

  def LW(t: Reg, i: Int, s: Reg): Word = {
    Word(Bits(s"100011${binreg(s)}${binreg(t)}${encodeImm(i)}"))
  }

  def SW(t: Reg, i: Int, s: Reg): Word = {
    Word(Bits(s"101011${binreg(s)}${binreg(t)}${encodeImm(i)}"))
  }

  def SLT(d: Reg, s: Reg, t: Reg): Word = {
    Word(Bits(s"000000${binreg(s)}${binreg(t)}${binreg(d)}00000101010"))
  }

  def SLTU(d: Reg, s: Reg, t: Reg): Word = {
    Word(Bits(s"000000${binreg(s)}${binreg(t)}${binreg(d)}00000101011"))
  }

  def BEQ(s: Reg, t: Reg, i: Int): Word = {
    Word(Bits(s"000100${binreg(s)}${binreg(t)}${encodeImm(i)}"))
  }

  def BNE(s: Reg, t: Reg, i: Int): Word = {
    Word(Bits(s"000101${binreg(s)}${binreg(t)}${encodeImm(i)}"))
  }

  def JR(s: Reg): Word = {
    Word(Bits(s"000000${binreg(s)}000000000000000001000"))
  }

  def JALR(s: Reg): Word = {
    Word(Bits(s"000000${binreg(s)}000000000000000001001"))
  }

  /** The `setMem` method in `State` loads one word into a specific memory location identified by
    * a given address. Implement this extended `setMem` method that takes a sequence of `words` and writes them
    * into successive memory locations in `inputState` starting with a given `startingAddress`. Return the
    * resulting state.
    */
  def setMem(words: Seq[Word], inputState: State = State(), startingAddress: Word = Word.zero): State = {
    require(decodeUnsigned(startingAddress) + words.size*4 <= decodeUnsigned(CPU.maxAddr))
    val (finalSt, _) = words.foldLeft((inputState, startingAddress)) {
      case ((st, addr), word) =>
        (st.setMem(addr, word), nextWord(addr))
    }
    finalSt
  }

  /** You can use this helper method to test the following programs that you will write. It loads the
    * given `words` into memory, writes the specified values to registers 1 and 2, and then runs
    * the CPU on the `words` that were loaded.
    */
  def loadAndRun(words: Seq[Word], register1: Word = Word.zero, register2: Word = Word.zero): (State, Trace) = {
    val initialState =
      setMem(words)
        .setReg(1, register1)
        .setReg(2, register2)
    CPU.run(initialState)
  }
}
