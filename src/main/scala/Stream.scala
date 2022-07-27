sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  def headOptionWithFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def toList: List[A] = {
    @scala.annotation.tailrec
    def loop(s: Stream[A], accu: List[A]): List[A] = s match {
      case Empty      => accu
      case Cons(h, t) => loop(t(), h() :: accu)
    }

    loop(this, Nil).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  def takeWithUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), x) if x > 0 => Some((h(), (t(), x - 1)))
      case _                        => None
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _                    => empty
  }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _                    => None
    }

  @scala.annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  // def exists2(p: A => Boolean): Boolean = this match {
  //   case Cons(h, t) => p(h()) || t().exists(p)
  //   case _ => false
  // }

  def exists(p: A => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(s: Stream[A]): Boolean = s match {
      case Cons(h, t) if p(h()) => true
      case Cons(_, t)           => loop(t())
      case _                    => false
    }

    loop(this)
  }

  // def forAll(p: A => Boolean): Boolean = this match {
  //   case Cons(h, t) => p(h()) && t().forAll(p)
  //   case Empty      => true
  // }

  // def forAll(p: A => Boolean): Boolean =
  //   foldRight(true)(p(_) && _)

  def forAll(p: A => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(s: Stream[A]): Boolean = s match {
      case Cons(h, t) =>
        if (p(h()))
          loop(t())
        else
          false
      case _ => true
    }

    loop(this)
  }

  @scala.annotation.tailrec
  final def foldLeft[B](z: => B)(f: (=> B, A) => B): B = this match {
    case Empty      => z
    case Cons(h, t) => t().foldLeft(f(z, h()))(f)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if (p(a))
        cons(a, b)
      else
        empty
    )

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def mapWithUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    }

  // def mapWithUnfold[B](f: A => B): Stream[B] =
  //   unfold(this)(s => s.headOption match {
  //     case Some(a) => Some((f(a), s.drop(1)))
  //     case _          => None
  //   })

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if (p(a)) cons(a, b)
      else b
    )

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
      case _                     => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    this.zipWith(s)(_ == _).forAll(x => x)

  def tails: Stream[Stream[A]] =
    unfold((this, 1)) {
      case (Cons(h, t), 1) => Some((cons(h(), t()), (t(), 1)))
      case (Empty, 1)      => Some((empty, (empty, 0)))
      case _               => None
    }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // def constant[A](a: A): Stream[A] = {
  //   lazy val infinite: Stream[A] = cons(a, infinite)
  //   infinite
  // }

  // def from(n: Int): Stream[Int] = {
  //   lazy val ints: Stream[Int] = cons(n, from(n + 1))

  //   ints
  // }

  // def fibs: Stream[Int] = {
  //   def loop(accu1: Int, accu2: Int): Stream[Int] = {
  //     lazy val ints: Stream[Int] =
  //       cons(accu1 + accu2, loop(accu2, accu1 + accu2))

  //     ints
  //   }

  //   cons(0, cons(1, loop(0, 1)))
  // }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    lazy val stream: Stream[A] = f(z) match {
      case None         => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

    stream
  }
  // def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
  //   case None         => empty[A]
  //   case Some((a, s)) => cons(a, unfold(s)(f))
  // }

  def fibs: Stream[Int] =
    unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def from(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def constant[A](a: A): Stream[A] =
    unfold(a)(s => Some((s, s)))

}
