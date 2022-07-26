sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    @scala.annotation.tailrec
    def loop(s: Stream[A], accu: List[A]): List[A] = s match {
      case Empty => accu
      case Cons(h, t) => loop(t(), h() :: accu)
    }

    loop(this, Nil).reverse
  }

  def take(n: Int): Stream[A] = {
    @scala.annotation.tailrec
    def loop(x: Int, s: Stream[A], accu: Stream[A]): Stream[A] = s match {
      case Cons(h, t) if n > 1 => loop(n - 1, t(), cons(h(), accu))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => this
    }

    loop(n, this, Empty)
  }


  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }



  @scala.annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // def exists2(p: A => Boolean): Boolean = this match {
  //   case Cons(h, t) => p(h()) || t().exists(p)
  //   case _ => false
  // }

  def exists(p: A => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(s: Stream[A]): Boolean = s match {
      case Cons(h, t) if p(h()) => true
      case Cons(_, t) => loop(t())
      case _ => false
    }

    loop(this)
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }


  @scala.annotation.tailrec
  final def foldLeft[B](z: => B)(f: (=> B, A) => B): B = this match {
    case Empty => z
    case Cons(h, t) => t().foldLeft(f(z, h()))(f)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if (p(a))
        cons(a, b)
      else
        empty
    )

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
}
