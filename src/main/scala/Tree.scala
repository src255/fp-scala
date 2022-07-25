sealed trait Tree[+A] {
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

  def depth: Int = this match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (l.depth max r.depth)
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  def fold[B](f: A => B)(g: (B, B) => B): B = this match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(l.fold(f)(g), r.fold(f)(g))
  }
}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]




object Tree {
  implicit class IntTree(val t: Tree[Int]) extends AnyVal {
    def maximum: Int = t match {
      case Leaf(a) => a
      case Branch(l, r) => l.maximum max r.maximum
    }
  }
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)


  def maximum2(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)


  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 0)(1 + _ max _)

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}