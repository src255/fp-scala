package hide

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(a, as) => a + sum(as)
  }

  def product(l: List[Double]): Double = l match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(a, as) => a * product(as)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](head: A, l: List[A]): List[A] =
    Cons(head, tail(l))

  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      if (n == 0)
        l
      else
        drop(t, n - 1)
    }
  }

  @scala.annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      if (f(h))
        dropWhile(t)(f)
      else
        l
    }
  }

  // def append2[A](l1: List[A], l2: List[A]): List[A] = l1 match {
  //   case Nil => l2
  //   case Cons(h, t) => Cons(h, append2(t, l2))
  // }


  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  @scala.annotation.tailrec
  def foldLeft[A, B](l: List[A], accu: B)(f: (B, A) => B): B = l match {
    case Nil => accu
    case Cons(h, t) => foldLeft(t, f(accu, h))(f)
  }

  def foldRight[A, B](l: List[A], accu: B)(f: (A, B) => B) = {
    foldLeft(reverse(l), accu)((b, a) => f(a, b))
  }

  // def foldLeft2[A, B](l: List[A], accu: B)(f: (B, A) => B) =
  //   foldRight(reverse(l), accu)((a, b) => f(b, a))

  // def foldRight2[A, B](l: List[A], accu: B)(f: (A, B) => B): B = l match {
  //   case Nil => accu
  //   case Cons(h, t) => f(h, foldRight(t, accu)(f))
  // }


  def sum2(l: List[Int]) = {
    foldLeft(l, 0)(_ + _)
  }

  def prod2(l: List[Int]) = {
    foldLeft(l, 1)(_ * _)
  }

  def prod2(l: List[Double]) = {
    foldLeft(l, 1.0)(_ * _)
  }


  def length[A](l: List[A]): Int = {
    foldLeft(l, 0)((b, a) => b + 1)
  }
  // def reverse[A](l: List[A]): List[A] =
    //   foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => append(reverse(t), Cons(h, Nil))
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)(Cons(_, _))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append(_, _))
  }

  def increment(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, increment(t))
  }

  def stringify(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, stringify(t))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      if (f(h))
        Cons(h, filter(t)(f))
      else
        filter(t)(f)
    }
  }

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => flatMap(l)(i => if (f(i)) Cons(i, Nil) else Nil)
  }

  def evensOnly(l: List[Int]): List[Int] = {
    filter(l)(_ % 2 == 0)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def startsWith(search: List[A], subSeq: List[A]): Boolean = (search, subSeq) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 && startsWith(t1, t2)
    }

    (l, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => startsWith(l, sub) || startsWith(t1, sub)
    }
  }
}