package com.github.abeln.tamarin


import com.github.abeln.tamarin.SymInstr._
import com.github.abeln.tamarin.Err.err
import com.github.abeln.tamarin.mips.CPU
import com.microsoft.z3._

/**
  * Interaction with the Z3 solver
  */
object Query {

  /** A solution is a sequence of values for the input registers. */
  sealed trait RegVal
  case object Unbound extends RegVal
  case class Fixed(r: Long, v: Long) extends RegVal

  type Soln = Seq[RegVal]

  /** Encapsulates solver state that's passed around implicitly. */
  private case class Ctx(z3: Context,
                         bv32: BitVecSort,
                         bv64: BitVecSort,
                         memsort: ArraySort)

  /**
    * Determines whether the given `trace` can be satisfied by an assignment to
    * the input registers.
    * */
  def solve(trace: Trace): Option[Soln] = {
    val ctx: Context = new Context
    val bv32 = ctx.mkBitVecSort(32)
    val bv64 = ctx.mkBitVecSort(64)
    // memory is a map from 32-bit bv to 32-bit bvs
    val mems = ctx.mkArraySort(bv32, bv32)
    implicit val context = Ctx(ctx, bv32, bv64, mems)

    val (reg2consts, memMap) = mkConsts(trace)

    val asserts = mkAsserts(trace, reg2consts, memMap)
    val solver = ctx.mkSolver()
    solver.add(asserts: _*)

    if (solver.check() == Status.SATISFIABLE) {
      val model = solver.getModel
      Some(inputRegs.toSeq map { r =>
        val interp = model.getConstInterp(reg2consts(r))
        if (interp == null) Unbound
        else Fixed(r.r, interp.toString.toLong.toInt) // TODO: find a better way to get the long value of a constant in the model
      })
    } else None
  }

  /**
    * Extracts the registers that are used in the `trace`, together with a boolean indicating whether they're
    * 32-bit registers (false = 64 bits).
    * Assumes that no register is used without first being initialized.
    * */
  private def regsIn(trace: Trace): Set[(Reg, Boolean)] = {
    val init: Set[(Reg, Boolean)] = initRegs.map((_, true))
    init ++ trace.flatMap {
      // Since every register must first be initialized, we can get away with only handling
      // the mutating instructions.
      case Add(d, _, _) => Seq((d, true))
      case Sub(d, _, _) => Seq((d, true))
      case Lw(t, _, _) => Seq((t, true))
      case Slt(d, _, _) => Seq((d, true))
      case SltU(d, _, _) => Seq((d, true))
      case Low32(d, _) => Seq((d, true))
      case High32(d, _) => Seq((d, true))
      case Quot(d, _, _) => Seq((d, true))
      case QuotU(d, _, _) => Seq((d, true))
      case Rem(d, _, _) => Seq((d, true))
      case RemU(d, _, _) => Seq((d, true))
      case Mult64(d, _, _) => Seq((d, false))
      case MultU64(d, _, _) => Seq((d, false))
      case _ => Seq()
    }.toSet
  }

  // NOTE: all helpers that take in a `Context` potentially mutate it, as a side effect.

  /** Information about how memory constants relate to each other. */
  private sealed trait MemOp
  private case class Select(src: ArrayExpr) extends MemOp // lw
  private case class Store(extend: ArrayExpr, dest: ArrayExpr) extends MemOp // sw

  /**
    * Build two maps:
    *   - registers to Z3 constants
    *   - sw & lw instructions to memory constants
    * Additionally, returns the initial memory constant.
    * */
  private def mkConsts(trace: Trace)(implicit c: Ctx): (Map[Reg, BitVecExpr], Map[Instr, MemOp]) = {
    val Ctx(ctx, bv32, bv64, memsort) = c

    val regMap = (Map.empty[Reg, BitVecExpr] /: regsIn(trace)) {
      case (map, (reg, is32)) =>
        map + (reg -> ctx.mkConst(reg.toString, if (is32) bv32 else bv64).asInstanceOf[BitVecExpr])
    }

    var i = 0
    def freshMem(): ArrayExpr = {
      i += 1
      ctx.mkConst(s"mem$i", memsort).asInstanceOf[ArrayExpr]
    }

    // Memory starts out as all-0s.
    val initMem = ctx.mkConstArray(bv32, ctx.mkNumeral(0, bv32))

    // Do a mini SSA conversion, just for memory ops.
    val (memMap, _) = ((Map.empty[Instr, MemOp], initMem) /: trace) {
      case ((mm, currMem), instr) =>
        instr match {
          case lw: Lw => (mm + (lw -> Select(currMem)), currMem)
          case sw: Sw =>
            val newMem = freshMem()
            (mm + (sw -> Store(currMem, newMem)), newMem)
          case _ => (mm, currMem)
        }
    }

    (regMap, memMap)
  }

  /** Computes all assertions generated by the `trace`, plus assertions for the initial state of the registers. */
  private def mkAsserts(trace: Trace, regMap: Map[Reg, BitVecExpr], memMap: Map[Instr, MemOp])(implicit c: Ctx): Seq[BoolExpr] = {
    val Ctx(ctx, bv32, bv64, _) = c
    def b32(o: Operand): BitVecExpr = o match {
      case r: Reg => regMap(r)
      case Lit(v) => ctx.mkNumeral(v, bv32).asInstanceOf[BitVecExpr]
    }
    def b64(o: Operand): BitVecExpr = o match {
      case r: Reg => regMap(r)
      case Lit(v) => ctx.mkNumeral(v, bv64).asInstanceOf[BitVecExpr]
    }

    val initAsserts = ((initRegs diff inputRegs) map { r =>
      val initVal = r match {
        case _ if r == savedPC => CPU.numTerminationPC
        case _ if r == stackPointer => CPU.numMaxAddr
        case _ => 0
      }

      ctx.mkEq(b32(r), b32(Lit(initVal)))
    }).toSeq

    def plus32(o1: Operand, o2: Operand): BitVecExpr = ctx.mkBVAdd(b32(o1), b32(o2))

    /** Generic (un)signed slt operator */
    def slt(cmp: (BitVecExpr, BitVecExpr) => BoolExpr, o1: Operand, o2: Operand): BitVecExpr = {
      ctx.mkITE(cmp(b32(o1), b32(o2)), b32(Lit(1)), b32(Lit(0))).asInstanceOf[BitVecExpr]
    }

    /** Widens a 32-bit bitvector to 64 bits */
    def widen64(o: Operand, signed: Boolean): BitVecExpr = {
      val bv = b32(o)
      if (bv.getSortSize != 32) err(s"Can only widen 32-bit vectors, this one has size ${bv.getSortSize}")
      else {
        if (signed) ctx.mkSignExt(32, b32(o))
        else ctx.mkZeroExt(32, b32(o))
      }
    }

    val traceAsserts = (Seq.empty[BoolExpr] /: trace) { (asserts, instr) =>
      (instr match {
        case Add(d, s, t) =>
          ctx.mkEq(b32(d), plus32(s, t))
        case Sub(d, s, t) =>
          ctx.mkEq(b32(d), ctx.mkBVSub(b32(s), b32(t)))
        case EqCond(s, t) =>
          ctx.mkEq(b32(s), b32(t))
        case NeqCond(s, t) =>
          ctx.mkNot(ctx.mkEq(b32(s), b32(t)))
        case Lw(t, offset, s) =>
          val Select(mem) = memMap(instr)
          ctx.mkEq(b32(t), ctx.mkSelect(mem, plus32(Lit(offset), s)))
        case Sw(t, offset, s) =>
          val Store(extend, mem) = memMap(instr)
          ctx.mkEq(mem, ctx.mkStore(extend, plus32(Lit(offset), s), b32(t)))
        case Slt(d, s, t) =>
          ctx.mkEq(b32(d), slt((o1, o2) => ctx.mkBVSLT(o1, o2), s, t))
        case SltU(d, s, t) =>
          ctx.mkEq(b32(d), slt((o1, o2) => ctx.mkBVULT(o1, o2), s, t))
        case Jalr(concretePC) =>
          ctx.mkEq(b32(savedPC), b32(Lit(concretePC)))
        case Mult64(d, s, t) =>
          // d is 64 bits
          ctx.mkEq(b64(d), ctx.mkBVMul(widen64(s, signed=true), widen64(t, signed=true)))
        case MultU64(d, s, t) =>
          // d is 64 bits
          ctx.mkEq(b64(d), ctx.mkBVMul(widen64(s, signed=false), widen64(t, signed=false)))
        case Low32(d, s) =>
          // s is 64 bits
          ctx.mkEq(b32(d), ctx.mkExtract(31, 0, b64(s)))
        case High32(d, s) =>
          // s is 64 bits
          ctx.mkEq(b32(d), ctx.mkExtract(63, 32, b64(s)))
        case Quot(d, s, t) =>
          ctx.mkEq(b32(d), ctx.mkBVSDiv(b32(s), b32(t)))
        case QuotU(d, s, t) =>
          ctx.mkEq(b32(d), ctx.mkBVUDiv(b32(s), b32(t)))
        case Rem(d, s, t) =>
          ctx.mkEq(b32(d), ctx.mkBVSRem(b32(s), b32(t)))
        case RemU(d, s, t) =>
          ctx.mkEq(b32(d), ctx.mkBVURem(b32(s), b32(t)))
        case _ => err(s"Unsupported instruction $instr")
      }) +: asserts
    }.reverse

    initAsserts ++ traceAsserts
   }
}
