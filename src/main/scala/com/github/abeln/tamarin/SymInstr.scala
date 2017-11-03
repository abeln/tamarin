package com.github.abeln.tamarin

/**
  * Symbolic instructions for MIPS
  */
object SymInstr {

  /** An operand is either a register or a literal (when the register has been concretized). */
  sealed trait Operand
  case class Reg(r: Int) extends Operand
  case class Lit(v: Long) extends Operand

  val maxReg = 31 // there are 31 addressable registers
  val addressable: Set[Reg] = (0 to maxReg).map(r => Reg(r)).toSet

  /** Special registers */
  val returnPC = Reg(31)
  val LO: Reg = freshRegister()
  val HI: Reg = freshRegister()
  val PC: Reg = freshRegister()

  private var newReg = maxReg

  /** Returns a new (virtual) register that's previously unused */
  def freshRegister(): Reg = {
    newReg += 1
    Reg(newReg)
  }

  /** A symbolic trace recorded while the program executed */
  type Trace = Seq[Instr]

  /** Marker trait for path conditions */
  sealed trait PathCond

  /** Symbolic instructions */
  sealed trait Instr
  case class Add(d: Reg, s: Operand, t: Operand) extends Instr
  case class Sub(d: Reg, s: Operand, t: Operand) extends Instr
  case class Mult(s: Operand, t: Operand) extends Instr
  case class MultU(s: Operand, t: Operand) extends Instr
  case class Div(s: Operand, t: Operand) extends Instr
  case class DivU(s: Operand, t: Operand) extends Instr
  case class Mfhi(d: Reg) extends Instr
  case class Mflo(d: Reg) extends Instr
  case class Lw(t: Reg, offset: Int, s: Operand) extends Instr
  case class Sw(t: Reg, offset: Int, s: Operand) extends Instr
  case class Slt(d: Reg, s: Operand, t: Operand) extends Instr
  case class SltU(d: Reg, s: Operand, t: Operand) extends Instr
  // Since we don't track the PC symbolically, we force you to concretize it for `jalr`.
  case class Jalr(s: Reg, concretePC: Int) extends Instr
  // Path conditions
  case class Beq(s: Operand, t: Operand, i: Int) extends Instr with PathCond
  case class Bne(s: Operand, t: Operand, i: Int) extends Instr with PathCond
  // jr doesn't show up here because we don't symbolically track the state of the PC.
}
