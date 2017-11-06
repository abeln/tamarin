package com.github.abeln.tamarin

import com.github.abeln.tamarin.SymInstr.{Lit, Operand, Reg}
import com.microsoft.z3._

/**
  * Helpers for constraint generation
  */
trait Constraints {
  val bv32: Sort
  val bv64: Sort

  type BVE = BitVecExpr

  /** assert(d == s + t) */
  def add32(d: BVE, s: BVE, t: BVE)(implicit ctx: Context): BoolExpr = {
    ctx.mkEq(d, ctx.mkBVAdd(s, t))
  }

  /** assert(l == r) */
  def eq32(l: BVE, r: BVE)(implicit ctx: Context): BoolExpr = {
    ctx.mkEq(l, r)
  }

  /** assert(d == s - t) */
  def sub32(d: BVE, s: BVE, t: BVE)(implicit ctx: Context): BoolExpr = {
    ctx.mkEq(d, ctx.mkBVSub(s, t))
  }
}
