package com.github.abeln.tamarin

import com.github.abeln.tamarin.SymInstr.{Instr, Trace}

/**
  * Maps traces to other traces
  */
abstract class TraceMap {

  protected def transform: PartialFunction[Instr, Trace]

  def apply(trace: Trace): Trace = {
    trace.flatMap { instr =>
      if (transform.isDefinedAt(instr)) transform(instr)
      else Seq(instr)
    }
  }
}

object TraceMap {
  implicit def instrToTrace(i: Instr): Trace = Seq(i)
}
