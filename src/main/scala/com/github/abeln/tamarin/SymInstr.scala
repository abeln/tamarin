package com.github.abeln.tamarin

/**
  * Symbolic instructions for MIPS
  */
object SymInstr {

  /** An operand is either a register or a literal (when the register has been concretized). */
  sealed trait Operand
  case class Reg(r: Long) extends Operand
  case class Lit(v: Long) extends Operand

  /** Special registers */
  val savedPC = Reg(31)
  val stackPointer = Reg(30)
  val LO: Reg = freshRegister()
  val HI: Reg = freshRegister()
  val TMP: Reg = freshRegister()

  val maxReg = 31 // there are 31 addressable registers
  val addressable: Set[Reg] = (0 to maxReg).map(r => Reg(r)).toSet
  val inputRegs: Set[Reg] = Set(Reg(1), Reg(2))
  // TODO: extend to multiple output registers?
  val outputReg = Reg(3)
  val initRegs: Set[Reg] = addressable ++ Set(LO, HI, TMP)

  private var newReg = maxReg

  /** Returns a new (virtual) register that's previously unused */
  def freshRegister(): Reg = {
    newReg += 1
    Reg(newReg)
  }

  /** A symbolic trace recorded while the program executed */
  type Trace = Seq[Instr]
  def trace(is: Instr*): Trace = Seq(is: _*)

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
  case class Lw(t: Reg, offset: Long, s: Operand) extends Instr
  case class Sw(t: Reg, offset: Long, s: Operand) extends Instr
  case class Slt(d: Reg, s: Operand, t: Operand) extends Instr
  case class SltU(d: Reg, s: Operand, t: Operand) extends Instr
  // Since we don't track the PC symbolically, we force you to concretize it for `jalr`.
  case class Jalr(concretePC: Long) extends Instr

  // The following aren't MIPS instructions, but are used to desugar MIPS instructions
  // into operations the solver can understand.
  /** Signed product s * t, where both operands are interpreted as 64-bit (signed) ints */
  case class Mult64(d: Reg, s: Operand, t: Operand) extends Instr
  /** Unsigned product s * t, where both operands are interpreted as 64-bit (unsigned) ints */
  case class MultU64(d: Reg, s: Operand, t: Operand) extends Instr
  /** Assigns the lowest 32-bits of `s` (assumed to be 64 bits) to `d` */
  case class Low32(d: Reg, s: Reg) extends Instr
  /** Assigns the highest 32-bits of `s` (assumed to be 64 bits) to `d` */
  case class High32(d: Reg, s: Reg) extends Instr
  /** Integer division */
  case class Quot(d: Reg, s: Operand, t: Operand) extends Instr
  /** Unsigned integer division */
  case class QuotU(d: Reg, s: Operand, t: Operand) extends Instr
  /** Remainder */
  case class Rem(d: Reg, s: Operand, t: Operand) extends Instr
  /** Unsigned remainder */
  case class RemU(d: Reg, s: Operand, t: Operand) extends Instr

  def assign(d: Reg, s: Operand) = Add(d, s, Lit(0))

  // Path conditions
  case class EqCond(s: Operand, t: Operand) extends Instr with PathCond
  case class NeqCond(s: Operand, t: Operand) extends Instr with PathCond
  // jr doesn't show up here because we don't symbolically track the state of the PC.
}
