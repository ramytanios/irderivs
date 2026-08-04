package lib

import lib.utils.BinarySearch.*

import scala.Ordering.Implicits.*
import scala.annotation.tailrec

object utils:

  enum BinarySearch:
    case Found(i: Int)
    case InsertionLoc(i: Int)

  def binarySearchBy[C, A: Ordering](cs: IndexedSeq[C], by: C => A)(elem: A): BinarySearch =
    @tailrec
    def go(from: Int, to: Int): BinarySearch =
      if from == to then InsertionLoc(from)
      else
        val idx = from + (to - from) / 2
        val a = by(cs(idx))
        if elem equiv a then Found(idx)
        else if elem < a then go(from, idx)
        else go(idx + 1, to)

    go(0, cs.size)

  def isStrictlyIncreasing[C: Ordering](cs: IndexedSeq[C]): Boolean =
    cs.indices.init.forall: i =>
      cs(i + 1) > cs(i)

extension [C](cs: IndexedSeq[C])
  def searchBy[A: Ordering](by: C => A)(elem: A): utils.BinarySearch =
    utils.binarySearchBy(cs, by)(elem)
  def isStrictlyIncreasing(using Ordering[C]): Boolean = utils.isStrictlyIncreasing(cs)

def uniform(from: Double, to: Double, n: Int): IndexedSeq[Double] =
  val step = (to - from) / n
  if step == 0.0 then IndexedSeq.empty[Double] else (0 to n).map(i => from + i * step)
