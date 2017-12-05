package com.github.abeln.tamarin

import com.github.abeln.tamarin.Query.{Fixed, RegVal, Soln, Unbound}
import com.github.abeln.tamarin.SymInstr._
import com.github.abeln.tamarin.mips.{CPU, Word}
import com.github.abeln.tamarin.mips.assembler.Assembler
import com.github.abeln.tamarin.mips.code.ProgramRepresentation.Code
import com.github.abeln.tamarin.mips.code.Transformations

import scala.annotation.tailrec

/**
  * Concolic testing of MIPS programs
  */
object Concolic {

  type Program = Seq[Code]

  /** A stateful trace contains stateful instructions */
  private type StTrace = Seq[StInstr]

  /**
    * Augments an instruction with information on whether its negation was already explored (flipped).
    * Only path conditions will be flipped: the field will be unused for all other instruction types.
    *
    * @param instr the underlying path condition
    * @param flipped whether the negation of the condition was already explored
    */
  private case class StInstr(instr: Instr, flipped: Boolean)

  /** The default max depth of the explored path conditions. Larger means more testing, but slower. */
  val pathCondDepth = 50

  /** Default number of steps to run the program for in each iteration */
  val defaultFuel = 10000

  /** Default values for input registers */
  private val defaultInputs: Seq[Long] = Seq.fill(inputRegs.size)(42)

  /** The state of a program within the concolic testing engine. */
  private sealed trait ProgramSt
  /** The program's state space has been completely explored */
  private case object Done extends ProgramSt
  /** The program can still execute further. `trace` denotes the current trace of path conds. */
  private case class NotDone(prog: Program, trace: StTrace) extends ProgramSt

  /** The result of an equivalence check via concolic testing */
  sealed trait Res
  /** The two programs are possibly equivalent (no difference in output was detected) */
  case object MaybeEquiv extends Res
  /** The two programs are _definitely not_ equivalent, together with input/output witnesses */
  case class NotEquiv(inputs: Seq[Long], ref: Long, cand: Long) extends Res

  /** A version of [[NotEquiv]] that uses 'driver' and 'verifier' (varying) as opposed to 'ref' and 'cand' (constant) */
  private case class Different(inputs: Seq[Long], driverRes: Long, verifierRes: Long)

  /**
    * Entry point to the concolic tester. Given two programs, finds out if they're not equivalent.
    *
    * @param reference the reference program
    * @param candidate the candidate program
    * @param depth     overrides the [[pathCondDepth]], indicating how thoroughly we explore the state space
    * @return whether the programs are not equivalent, or possibly equivalent.
    */
  def compare(reference: Program, candidate: Program)(implicit depth: Int = pathCondDepth): Res = {
    /** Takes one step in the comparison of the two input programs. Alternates between each of them at every step. */
    @tailrec def step(refSt: ProgramSt, candSt: ProgramSt, turn: Boolean): Res = {
      (refSt, candSt) match {
        case (Done, Done) => MaybeEquiv
        case _ =>
          val (driver, verifier) = if (turn) (refSt, candidate) else (candSt, reference)
          stepAux(driver, verifier) match {
            case Left(Different(inps, resDriver, resVerifier)) =>
              if (turn) {
                NotEquiv(inps, resDriver, resVerifier)
              } else {
                NotEquiv(inps, resVerifier, resDriver)
              }
            case Right(newDriverSt) =>
              if (turn) {
                step(newDriverSt, candSt, !turn)
              } else {
                step(refSt, newDriverSt, !turn)
              }
          }
      }
    }

    /**
      * The core of the testing algorithm: executes the given driver for one step, verifying its output against
      * the verifier.
      *
      * @return a counterexample, or the new program state
      **/
    def stepAux(driverSt: ProgramSt, verifier: Program): Either[Different, ProgramSt] = {
      driverSt match {
        case Done => Right(Done)
        case NotDone(driver, trace) =>
          findTrace(trace) match {
            case None => Right(Done)
            case Some((nTrace, soln)) =>
              val (r1, r2) = getInputs(soln)
              val driverRes = runProg(driver, r1, r2)
              val verifierRes = runProg(verifier, r1, r2)
              if (!isCompatible(driverRes, verifierRes)) {
                val ExecDone(driverVal, _) = driverRes
                val ExecDone(verifierVal, _) = verifierRes
                Left(Different(Seq(r1, r2), driverVal, verifierVal))
              } else {
                adaptTrace(nTrace, getTrace(driverRes)) match {
                  case None => Right(Done) // TODO: implement restarts
                  case Some(adaptedTrace) => Right(NotDone(driver, adaptedTrace))
                }
              }
          }
      }
    }

    /***
      * Returns an initial trace for the driver program (using the provided input register values),
      * or a counter-example if we got lucky.
      **/
    def init(driver: Program, verifier: Program, r1: Long, r2: Long): Either[Different, ProgramSt] = {
      val driverRes = runProg(driver, r1, r2)
      val verifierRes = runProg(verifier, r1, r2)
      if (!isCompatible(driverRes, verifierRes)) {
        val ExecDone(driverVal, _) = driverRes
        val ExecDone(verifierVal, _) = verifierRes
        Left(Different(Seq(r1, r2), driverVal, verifierVal))
      } else {
        Right(NotDone(driver, getTrace(driverRes) map {instr => StInstr(instr, flipped = false)}))
      }
    }

    // TODO: don't hardcode the inputs
    val r1 = defaultInputs(0)
    val r2 = defaultInputs(1)

    init(reference, candidate, r1, r2) match {
      case Left(Different(inps, dr, ver)) => NotEquiv(inps, dr, ver)
      case Right(refSt) =>
        init(candidate, reference, r1, r2) match {
          case Left(Different(inps, dr, ver)) => NotEquiv(inps, ver, dr) // flip the order
          case Right(candSt) =>
            step(refSt, candSt, turn = true)
        }
    }
  }

  /** Result of a program run */
  private sealed trait ExecRes
  private case class ExecDone(res: Long, trace: Trace) extends ExecRes
  private case class ExecStopped(trace: Trace) extends ExecRes

  /** Runs a program, returning the value of the output register + the symbolic trace */
  private def runProg(prog: Program, r1: Long, r2: Long)(implicit depth: Int): ExecRes = {
    val res = Assembler.loadAndRun(
      Transformations.toMachineCode(prog),
      Word(Assembler.encodeSigned(r1)),
      Word(Assembler.encodeSigned(r2)),
      defaultFuel
    )
    res match {
      case CPU.Done(st, tr) =>
        ExecDone(Assembler.decodeSigned(st.reg(outputReg.r).toSeq), TraceTransform.go(tr))
      case CPU.NotDone(tr) =>
        ExecStopped(TraceTransform.go(tr))
    }
  }

  /**
    * Determines whether two results are 'compatible'.
    * The only incompatible results are two [[ExecDone]] with different values in the `res` field.
    * In particular, if either of the results is [[ExecStopped]], then we conservatively assume the result
    * would've been the same if we let the program run for long enough.
    **/
  private def isCompatible(res1: ExecRes, res2: ExecRes): Boolean = {
    (res1, res2) match {
      case (ExecDone(val1, _), ExecDone(val2, _)) if val1 != val2 => false
      case _ => true
    }
  }

  private def getTrace(res: ExecRes): Trace = {
    res match {
      case ExecDone(_, tr) => tr
      case ExecStopped(tr) => tr
    }
  }

  /** Interface to the Query module that first simplifies the trace. */
  private def query(trace: StTrace): Option[Soln] = {
    Query.solve(stripFlipped(trace))
  }

  /** Finds the deepest path condition that can be negated, to force a (potentially) new path. */
  private def findTrace(trace: StTrace): Option[(StTrace, Soln)] = {
    (prefixes(trace) :\ None.asInstanceOf[Option[(StTrace, Soln)]]) {
      case (_, res: Some[(StTrace, Soln)]) => res // if we already have a solution, stick to it, since we want the largest prefix
      case (candTrace, None) =>
        val StInstr(instr, flipped) :: rTrace = candTrace.reverse
        if (!isPC(instr) || flipped) {
          None // can't flip
        } else {
          val nTrace = (StInstr(neg(instr), flipped = true) :: rTrace).reverse
          query(nTrace) match {
            case None => None
            case Some(sol) => Some((nTrace, sol))
          }
        }
    }
  }

  /** Produces the prefixes of `seq` in increasing order */
  private def prefixes[A](seq: Seq[A]): Seq[Seq[A]] = seq.indices.map(i => seq.take(i + 1))

  /** Negate the instruction if it's a pathc condition */
  private def neg(instr: Instr): Instr = {
    instr match {
      case EqCond(s, t) => NeqCond(s, t)
      case NeqCond(s, t) => EqCond(s, t)
      case _ => Err.err(s"Can't flip a non path cond $instr")
    }
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

  private def stripFlipped(trace: StTrace): Trace = trace.map(_.instr)

  /** Verifies that a new trace matches a reference one, converting the new one to a `StTrace` in the process. */
  private def adaptTrace(refTrace: StTrace, actualTrace: Trace): Option[StTrace] = {
    def filterPC(tr: Trace): Trace = tr.filter(isPC)
    val refPC = filterPC(stripFlipped(refTrace))
    val actualPC = filterPC(actualTrace)

    if (actualPC.startsWith(refPC) || refPC.startsWith(actualPC)) {
      val (flippedMap, _) = ((Map.empty[Int, Boolean], 0) /: refTrace) {
        case ((fmap, idx), instr) =>
          if (isPC(instr.instr)) {
            (fmap + (idx -> instr.flipped), idx + 1)
          } else {
            (fmap, idx)
          }
      }
      val (rActual, _) = ((Seq.empty[StInstr], 0) /: actualTrace) {
        case ((rTrace, idx), instr) =>
          if (isPC(instr)) {
            val stInstr = if (flippedMap.contains(idx)) {
              StInstr(instr, flipped = flippedMap(idx))
            } else {
              StInstr(instr, flipped = false) // this is a new path condition not previously seen
            }
            (stInstr +: rTrace, idx + 1)
          } else {
            (StInstr(instr, flipped = false) +: rTrace, idx) // can only flip path conds
          }
      }
      Some(rActual.reverse)
    } else {
      None // Our prediction for what the new trace should be is off
    }
  }
}
