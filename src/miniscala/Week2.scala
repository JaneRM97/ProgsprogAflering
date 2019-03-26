package miniscala

import miniscala.Week2.length

object Week2 {

  sealed abstract class IntList
  case object Nil extends IntList {override def toString: String = ""}

  case class Cons(x: Int, xs: IntList) extends IntList {
    override def toString: String =
      s"$x, ${xs.toString}"
  }


  def main(args: Array[String]): Unit = {
    val a = Cons(79, Cons(13, Nil))
    println(a)
    //print(length(a))
    //println(append(a,10))
    //println(square(Cons(2, Cons(5, Cons(3, Nil)))))
    //println(ordered(Cons(2, Cons(5, Cons(7, Nil)))))
  }

  def merge(xs: IntList, ys: IntList): IntList = (xs, ys) match {
    case (Nil, Nil) => Nil // hmm redundant?
    case (Nil,_) => ys
    case (_,Nil) => xs
    case (Cons(x, xs2), Cons(y,ys2)) =>
      if(x <= y) Cons(x, merge(xs2,ys)) else Cons(y, merge(xs,ys2))
  }

  def split(xs: IntList, n: Int): (IntList, IntList) =
    if(length(xs) <= n ) (Nil, xs) else (xs,n) match {
      case (_, 0) => (Nil, xs)
      case (Cons(y, ys), n) =>
        val mussi = split(ys, n-1)
        (Cons(y, mussi._1), mussi._2)
    }

  def mergeSort(xs: IntList): IntList = {
    val n = length(xs) / 2
    if (n == 0) xs
    else {
      val (left, right) = split(xs, n)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  /*def permuted(xs: IntList, ys: IntList): Boolean = {
      case(_, Nil) => ys == Nil
      case (Nil, _) => xs == Nil
      case (Cons(x, vs), Cons(y, zs)) =>
        permuted(vs, removeAndReturn(x, ys))
  }
  def removeAndReturn(x: Int, xs: IntList): IntList = (x, xs) match {
    case(_, Nil) => xs
    case (x, Cons(y, ys)) => if(x == y) zs else Cons(y, removeAndReturn(x, ys))
  }*/

  def length(xs: IntList): Int = xs match {
    case Nil => 0
    case Cons(_, ys) => 1 + length(ys)
  }

  def append(xs: IntList, x: Int): IntList = {
    println(s"appending $xs and $x")
    xs match {
      case Nil => Cons(x,Nil)
      case Cons(y,ys) => Cons(y,append(ys, x))
    }
  }

  def square(xs: IntList): IntList = xs match  {
    case Nil => Nil
    case Cons(y, ys) => Cons(y*y,square(ys))
  }


  def ordered(xs: IntList): Boolean = xs match {
    case Nil => true
    case Cons(y, ys) => ys match {
      case Nil => true
      case Cons(z,_) =>
        (y<=z) && ordered(ys)

    }
  }

  



}