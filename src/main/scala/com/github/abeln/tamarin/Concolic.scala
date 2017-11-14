package com.github.abeln.tamarin

import com.github.abeln.tamarin.Query.{Fixed, RegVal, Soln, Unbound}
import com.github.abeln.tamarin.SymInstr._
import com.github.abeln.tamarin.mips.Word
import com.github.abeln.tamarin.mips.assembler.Assembler
import com.github.abeln.tamarin.mips.code.ProgramRepresentation.Code
import com.github.abeln.tamarin.mips.code.Transformations

import scala.annotation.tailrec

/**
  * Concolic testing of MIPS programs
  */
object Concolic {

  type Program = Seq[Code]

  /** Stateful trace containing stateful instructions */
  private type StTrace = Seq[StInstr]

  /** The default max depth of the explored path conditions. Larger means more testing, but slower. */
  val defaultDepth = 42

  /** Default values for input registers */
  private val defaultInputs: Seq[Long] = Seq.fill(inputRegs.size)(7)

  /**
    * A state in the execution of the concolic testing engine.
    *
    * @param driver the program that `drives` the execution (i.e. the one whose path conditions we care about)
    * @param passenger the program whose results must match those of the driver. It doesn't drive any execution decisions.
    * @param trace the sequence of symbolic instructions taken in the previous execution of `driver`.
    */
  private case class ExecState(driver: Program, passenger: Program, trace: StTrace)

  /**
    * Augments a symbolic instruction with information on whether its negation was already explored
    * Note that, in practice, this applies only to path conditions (but it's not constrained by the types).
    *
    * @param instr the underlying instruction
    * @param flipped whether the negation of the condition was already explored (only for path conds)
    */
  private case class StInstr(instr: Instr, flipped: Boolean)

  /**
    * General state of the concolic engine, with one execution state for each of the reference and candidate programs.
    * @param turn indicates which program we should "drive" at each step.
    * @param freezeTurn whether we should stop flipping turn (used when one of the two programs is completely explored)
    **/
  private case class State(refState: ExecState, candState: ExecState, turn: Boolean, freezeTurn: Boolean)

  /** The result of an equivalence check via concolic testing */
  sealed trait Res
  /** The two programs are possibly equivalent */
  case object MaybeEquiv extends Res
  /** The two programs are _definitely not_ equivalent, together with input/output witnesses */
  case class NotEquiv(inputs: Seq[Long], outputRef: Long, outputCand: Long) extends Res

  /**
    * Entry point to the concolic tester. Given two programs, finds out if they're not equivalent.
    *
    * @param reference the reference program
    * @param candidate the candidate program
    * @param depth overrides the [[defaultDepth]], indicating how thoroughly we explore the state space
    *
    * @return whether the programs are not equivalent, or possibly equivalent.
    */
  def compare(reference: Program, candidate: Program, depth: Int = defaultDepth): Res = {
    /** Takes one step in the comparison of the two input programs. Alternates between each of them at every step. */
    @tailrec def step(st: State): Res = {
      val State(ref, cand, turn, freeze) = st
      val curr = if (turn) ref else cand
      stepAux(curr) match {
        case Right(MaybeEquiv) =>
          if (freeze) MaybeEquiv // we've exhaused the branches on both programs
          else step(State(ref, cand, !turn, freezeTurn = true)) // execute only one program from now on
        case Right(ne@NotEquiv(inputs, o1, o2)) =>
          if (turn) ne
          else NotEquiv(inputs, o2, o1) // flip the outputs so that the reference and cand are properly marked
        case Left(newExecSt) =>
          val nturn = if (freeze) turn else !turn
          val nextSt = if (turn) {
            State(newExecSt, cand, nturn, freezeTurn = freeze)
          } else {
            State(ref, newExecSt, nturn, freezeTurn = freeze)
          }
          step(nextSt)
      }
    }

    /**
      * The core of the testing algorithm: executes the given driver for one step.
      * If the driver can take one step, it returns either the new `Left(ExecState)`, or a counter-example `Right(NotEquiv)`.
      * If the driver can't take any more steps, returns `Right(MaybeEquiv)`.
      **/
    def stepAux(execSt: ExecState): Either[ExecState, Res] = {
      val ExecState(driver, pass, trace) = execSt
      findTrace(trace, depth) match {
        case None => Right(MaybeEquiv)
        case Some((candTrace, inputs)) =>
          val (reg1, reg2) = getInputs(inputs)
          runTwo(driver, pass, reg1, reg2) match {
            case Left(ne) if ne.isInstanceOf[NotEquiv] => Right(ne)
            case Right((driverTrace, _)) =>
              adaptTrace(candTrace, driverTrace) match {
                case None => Right(MaybeEquiv) // TODO: implement restarts when traces don't match
                case Some(newTrace) =>
                  Left(ExecState(driver, pass, newTrace))
              }
          }
      }
    }

    init(reference, candidate) match {
      case Left(ne) if ne.isInstanceOf[NotEquiv] => ne
      case Right((refTrace, candTrace)) =>
        val refState = ExecState(reference, candidate, refTrace)
        val candState = ExecState(candidate, reference, candTrace)
        step(State(refState, candState, turn = true, freezeTurn = false))
    }
  }

  /** Result of a program run */
  private case class RunResult(res: Long, trace: Trace)

  /** Runs a program, returning the value of the output register + the symbolic trace */
  private def runProg(prog: Program, r1: Long, r2: Long): RunResult = {
    val (finalSt, trace) = Assembler.loadAndRun(
      Transformations.toMachineCode(prog),
      Word(Assembler.encodeSigned(r1)),
      Word(Assembler.encodeSigned(r2))
    )
    RunResult(Assembler.decodeSigned(finalSt.reg(outputReg.r).toSeq), trace)
  }

  private def runTwo(ref: Program, cand: Program, reg1: Long, reg2: Long): Either[NotEquiv, (Trace, Trace)] = {
    val RunResult(resRef, traceRef) = runProg(ref, reg1, reg2)
    val RunResult(resCand, traceCand) = runProg(cand, reg1, reg2)
    if (resRef != resCand) Left(NotEquiv(Seq(reg1, reg2), resRef, resCand))
    else Right((traceRef, traceCand))
  }

  /** Returns two initial traces for the programs, or a counter-example if we got lucky. */
  private def init(ref: Program, cand: Program): Either[NotEquiv, (StTrace, StTrace)] = {
    // TODO: don't hardcode these
    val r1Default = defaultInputs(0)
    val r2Default = defaultInputs(1)
    runTwo(ref, cand, r1Default, r2Default) match {
      case Left(ne) => Left(ne)
      case Right((traceRef, traceCand)) =>
        Right((addFlip(traceRef), addFlip(traceCand)))
    }
  }

  /** Interface to the Query module that first simplifies the trace. */
  private def query(trace: StTrace): Option[Soln] = {
    val tr = trace map (_.instr)
    val transform = Desugar andThen SSAConv
    Query.solve(transform(tr))
  }

  /** Finds the deepest path condition (<= maxDepth) that can be negated, to force a (potentially) new path. */
  private def findTrace(trace: StTrace, maxDepth: Int): Option[(StTrace, Soln)] = {
    // TODO: this is super inefficient: find a better way
    var i = 0
    var depth = 0
    while (i < trace.size && depth < maxDepth) {
      trace(i) match {
        case instr: Instr if isPathCond(instr) =>
          depth += 1
        case _ =>
      }
      i += 1
    }
    val subtrace = trace.slice(0, i)

    var found = false
    var newtrace: StTrace = Seq()
    var res: Option[(StTrace, Soln)] = None

    i = subtrace.size - 1
    while (i >= 0 && !found) {
      subtrace(i) match {
        case instr@StInstr(_, false) if isPathCond(instr) => // A path condition that hasn't already been flipped.
          newtrace = subtrace.slice(0, i) :+ StInstr(neg(subtrace(i).instr), flipped = true)
          query(newtrace) match {
            case Some(soln) =>
              res = Some((newtrace, soln))
              found = true
            case None =>
          }
        case _ =>
      }
      i -= 1
    }

    res
  }

  private def addFlip(trace: Trace): StTrace = trace map (i => StInstr(i, flipped = false))

  private def isPathCond(instr: StInstr): Boolean = instr.instr.isInstanceOf[PathCond]

  private def neg(pc: Instr): Instr = pc match {
    case EqCond(s, t) => NeqCond(s, t)
    case NeqCond(s, t) => EqCond(s, t)
    case _ => Err.err(s"Can't negate instruction $pc: not a path condition")
  }

  /** Gets the input register values from a `Soln` */
  private def getInputs(soln: Soln): (Long, Long) = {
    def getInp(r: RegVal): Long = r match {
      case Unbound => 0 // TODO: is 0 a good value?
      case Fixed(_, v) => v
    }
    // TODO: don't hardcode the indices
    (getInp(soln(0)), getInp(soln(1)))
  }

  /** Verifies that a new trace matches a reference one, converting the new one to a `StTrace` in the process. */
  private def adaptTrace(refTrace: StTrace, actualTrace: Trace): Option[StTrace] = {
    // TODO: optimize
    val conds = refTrace.filter(isPathCond)
    val res = collection.mutable.ArrayBuffer.empty[StInstr]
    var i = 0
    var currCond = 0
    while (i < actualTrace.size) {
      if (currCond >= conds.size || !actualTrace(i).isInstanceOf[PathCond]) {
        res +=  StInstr(actualTrace(i), flipped = false)
      } else {
        if (conds(currCond).instr != actualTrace(i)) {
          return None
        } else {
          res += conds(currCond)
          currCond += 1
        }
      }
      i += 1
    }
    Some(res)
  }
}
