package com.github.abeln.tamarin

/**
  * Entry point to Tamarin
  */
object Main {

  def main(args: Array[String]): Unit = {
    System.out.println(Query.query(Seq()))
  }
}