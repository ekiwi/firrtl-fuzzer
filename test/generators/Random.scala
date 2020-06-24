package generators

// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

import com.pholser.junit.quickcheck.random.SourceOfRandomness

trait Random {
  def nextInt(min: Int, max: Int) : Int
  def choose[T](items: Seq[T]) : T
  def nextBoolean() : Boolean
  def nextBigInteger(bits: Int): BigInt
}


class JUnitRandom(private val r: SourceOfRandomness) extends Random {
  override def nextBoolean() = r.nextBoolean()
  override def choose[T](items: Seq[T]) : T = {
    val a = scala.collection.JavaConverters.seqAsJavaList(items)
    r.choose(a)
  }
  override def nextInt(min: Int, max: Int) = r.nextInt(min, max)
  override def nextBigInteger(bits: Int) = r.nextBigInteger(bits)
}