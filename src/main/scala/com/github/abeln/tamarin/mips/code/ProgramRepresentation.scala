package com.github.abeln.tamarin.mips.code

import com.github.abeln.tamarin.mips.Word
import com.github.abeln.tamarin.mips.assembler.Assembler._
import com.github.abeln.tamarin.mips.assembler.Reg

/** Defines the elements that are composed to represent programs in the internal representation of the
  *  CS 241E compiler.
  */
object ProgramRepresentation {
  /* The following are used starting in Assignment 2. */

  /** A label representing a textual name for a given memory address. */
  class Label(name: String) {
    override def toString = s"Label($name)"
  }

  /** The overall abstract class representing a piece of code. It can be instantiated as any of the
    * subclasses that extend `Code`. */
  sealed abstract class Code

  /** A machine language instruction represented as a 32-bit `Word`. */
  case class CodeWord(word: Word) extends Code

  /** Enables the use of a `Word` wherever a `Code` is expected by wrapping the `Word` in a `CodeWord`. */
  implicit def toCodeWord(word: Word) = CodeWord(word)

  /** A definition of a label. The given label is associated with the memory address at which the
    * code immediately following the `Define` will be loaded in memory.
    */
  case class Define(label: Label) extends Code

  /** A use of a label. Once the addresses associated with all labels are known, each `Use` will be replaced
    * by a 32-bit word, the address that is associated with the given label.
    */
  case class Use(label: Label) extends Code

  /** A BEQ or BNE instruction whose target address is specified by a label. Once the addresses associated
    * with all labels are known, each `BeqBne` will be replaced by a 32-bit word representing a BEQ or BNE
    * instruction that transfers control to the given label if the associated condition is satisfied.
    * The `bits` parameter are the first 16 bits of the BEQ or BNE instruction to be generated. The last
    * 16 bits will be filled in with a branch offset that is determined once both the memory address
    * of the BEQ or BNE instruction and the memory address associated with the label are known.
    *
    * See the `beq` and `bne` methods in `ProgramRepresentation.scala` for a convenient way to create a BeqBne.
    */
  case class BeqBne(bits: Seq[Boolean], label: Label) extends Code

  /** Creates a `BeqBne` that generates a BEQ instruction that branches to the address associated with `label`. */
  def beq(s: Reg, t: Reg, label: Label): BeqBne = BeqBne(BEQ(s, t, 0).take(16), label)

  /** Creates a `BeqBne` that generates a BNE instruction that branches to the address associated with `label`. */
  def bne(s: Reg, t: Reg, label: Label): BeqBne = BeqBne(BNE(s, t, 0).take(16), label)

  /** A comment which is removed when the machine language code is generated. The `message` is retained in
    * a separate table so that it can be shown by the `Debugger` when it executes the generated code.
    */
  case class Comment(message: String) extends Code
}

