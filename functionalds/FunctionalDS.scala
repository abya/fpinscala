package fpinscala.functionalds

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

//companion object
object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new RuntimeException("error")
    case Cons(_, t) => t
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t)(f)
    case _ => l
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }
  
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init on empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
  
  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }
  
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f)) 
  }
  
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = 
    foldLeft(reverse(l), z)((b, a) => f(a, b))
    
  def sum2(l: List[Int]): Int = foldRight(l, 0)(_ + _)
  
  def product2(l: List[Double]): Double = foldRight(l, 1.0)(_ * _)
  
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, n) => n + 1)
  
  // my version
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def foo(lst: List[A], acc: B): B = lst match {
      case Nil => acc
      case Cons(h, t) => foo(t, f(acc, h))
    }
    foo(l, z)
  }
  
  @tailrec
  def foldLeft1[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft1(t, f(z, h))(f)
  }
  
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))
  
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)((elem, acc) => Cons(elem, acc))
  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = foldLeft(List.reverse(l), r)((acc, elem) => Cons(elem, acc))
  
  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])(append)
  
  // If you use foldLeft for add1, you need to reverse l before passing it to foldLeft, or use foldRightViaFoldLeft
  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a, b) => Cons(a+1, b))
  
  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((elem, acc) => Cons(elem.toString, acc))
  
  // for stack safety, use foldRightViaFoldLeft in map
  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((elem, acc) => Cons(f(elem), acc))
  
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))
  
  def filter[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)
  
  // use foldRightViaFoldLeft for stack safety.
  def filter1[A](l: List[A])(f: A => Boolean): List[A] = 
    foldRight(l, Nil:List[A])((elem, acc) => if (f(elem)) Cons(elem, acc) else acc)
    
  def addPairWise(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
  }
  
  def zipWith[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] = (l, r) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
  
  def addPairWise1(l: List[Int], r: List[Int]): List[Int] = zipWith(l, r)(_ + _)
  
  def startsWith[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith(t1, t2)
    case _ => false
  }
  
  @tailrec
  def hasSubSequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => false
    case Cons(h, t) if (startsWith(l, sub)) => true
    case Cons(h, t) => hasSubSequence(t, sub)
  }
}

object Test {
  def main(args: Array[String]) {
    //println(List.append(Cons(1, Cons(2, Nil)), Cons(3, Cons(4, Nil))))
    //println(List.init(Cons(1, Cons(2, Nil))))
    //println(    List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _)))
    //println(List.foldRight(List(1,2,3), 0)(_ + _))
    //println(List.appendViaFoldLeft(List(1,2,3), List(4,5,6)))
    //println(List.appendViaFoldRight(List(1,2,3), List(4,5,6)))
    //println(List.concat(List(List(1,2), List(3,4), List(5,6))))
    //println(List.add1(List(1,2,3)))
    //println(List.doubleToString(List(1,2,3)))
    //println(List.map(List(1,2,3))(_.toString))
  }
}