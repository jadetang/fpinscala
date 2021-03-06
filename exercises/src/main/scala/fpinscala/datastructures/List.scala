package fpinscala.datastructures

import scala.collection.mutable.ListBuffer

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new RuntimeException("empty List")
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new RuntimeException("empty List")
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw new RuntimeException("empty List")
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)


  def foldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(x, z))(f)

    }
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  }

  def sumViaFoldLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productViaFoldLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((_, acc) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((h, acc) => Cons(h, acc))

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldRight(reverse(l), z)(f)

  def appendViaFoldLeft[A](a: List[A], b: List[A]): List[A] = sys.error("todo")

  def appendViaFoldRight[A](a: List[A], b: List[A]): List[A] = foldRight(a, b)((h, acc) => Cons(h, acc))

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(appendViaFoldRight(_, _))
  }

  def add1(l: List[Int]): List[Int] = map(l)(_ + 1)

  def doubleToString(l: List[Double]): List[String] = map(l)(_.toString)

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => append(List(x), filter(xs)(f))
    case Cons(_, xs) => filter(xs)(f)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f(_)))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)((x: A) => if (f(x)) List(x) else Nil)

  def addPairwise(left:List[Int],right:List[Int]):List[Int] = (left,right) match {
    case (x,Nil) => Nil
    case (Nil,y) => Nil
    case (Cons(x,xs),Cons(y,ys))=>append(List(x+y),addPairwise(xs,ys))
  }

  def zipWith[A,B,C](left:List[A],right:List[B])(f:(A,B)=>C):List[C] = (left,right) match {
    case (x,Nil) => Nil
    case (Nil,y) => Nil
    case (Cons(x,xs),Cons(y,ys))=>append(List(f(x,y)),zipWith(xs,ys)(f))
  }

  def hasSubsequence[A](list:List[A],sub:List[A]):Boolean = sys.error("todo")

}