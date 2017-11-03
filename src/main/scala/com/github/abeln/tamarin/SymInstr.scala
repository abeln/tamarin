package com.github.abeln.tamarin

/**
  * Symbolic instructions for MIPS
  */
object SymInstr {

  /** An operand is either a register or a literal (when the register has been concretized). */
  sealed trait Operand
  case class Reg(r: Int) extends Operand
  case class Lit(v: Long) extends Operand

  /** A symbolic trace recorded while the program executed */
  type Trace = List[PathCond]

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
  case class Sltu(d: Reg, s: Operand, t: Operand) extends Instr
  // Path conditions
  case class Beq(s: Operand, t: Operand, i: Lit) extends Instr with PathCond
  case class Bne(s: Operand, t: Operand, i: Lit) extends Instr with PathCond
  // jr and jalr don't show up here because we don't symbolically track
  // the state of the PC.
}
