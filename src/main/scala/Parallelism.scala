// object for parallel computations
object Par {
  def unit[A](a: A): Par[A]
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def get[A](a: Par[A]): A
  def map2[A, B, C](a: => A, b: => B)(f: (A, B) => C): Par[C] = ???
  def fork[A](a: => Par[A]): Par[A] = ???
}
