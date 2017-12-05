# Tamarin 
<img src="https://github.com/abeln/tamarin/blob/master/logo.png" width="100" alt="logo of a tamarin monkey"> 

Tamarin is a tool for establishing program equivalence for MIPS programs.

That is, given two MIPS assembly programs, Tamarin returns
  * _distinct_ if the programs can be shown to compute different functions
  * _maybe equivalent_ otherwise

Tamarin uses [concolic testing](https://en.wikipedia.org/wiki/Concolic_testing).
