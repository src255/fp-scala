trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      val (b, rng3) = g(a)(rng2)

      (b, rng3)
    }

  def map[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = ra(rng)

      (f(a), rng2)
    }

  def _map[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)

      (f(a, b), rng3)
    }

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, nextRng) = rng.nextInt
    if (num == Int.MinValue)
      (0, nextRng)
    else if (num < 0)
      (-num, nextRng)
    else
      (num, nextRng)
  }

  def nonNegativeEven: Rand[Int] =
    map(r => nonNegativeInt(r))(i => i - i % 2)

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(int) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  def double(rng: RNG): (Double, RNG) = {
    val (num, nextRng) = rng.nextInt
    if (num < 0)
      (-num / (Int.MaxValue.toDouble + 2), nextRng)
    else
      (num / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def _double: Rand[Double] =
    map(int)(_.abs / (Int.MaxValue.toDouble + 2))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def _intDouble(rng: RNG): Rand[(Int, Double)] =
    both(int, _double)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def _doubleInt(rng: RNG): Rand[(Double, Int)] =
    both(_double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def sequence[A](xs: List[Rand[A]]): Rand[List[A]] =
    xs.foldRight(unit[List[A]](Nil)) { (next, acc) =>
      map2(next, acc)(_ :: _)
    }

  // def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  //   fs match {
  //     case Nil => rng => (Nil, rng)
  //     case h :: t =>
  //       rng => {
  //         val (a, rng2) = h(rng)
  //         val (as, rng3) = sequence(t)(rng2)

  //         (a :: as, rng3)
  //       }
  //   }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0)
      (Nil, rng)
    else {
      val (newInt, newRng) = rng.nextInt
      val res = ints(count - 1)(newRng)
      (newInt :: res._1, res._2)
    }

  def _ints(count: Int): Rand[List[Int]] =
    rng => sequence(List.fill(count)(int))(rng)
}

import State._

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)

      f(a).run(s2)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State {
  type Rand[+A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](Nil)) { (next, acc) =>
      next.map2(acc)(_ :: _)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def processInput: Input => (Machine => Machine) = i =>
    s =>
      (i, s) match {
        case (_, Machine(_, 0, _))        => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _))  => s
        case (Coin, Machine(true, candy, coins)) =>
          Machine(false, candy, coins + 1)
        case (Turn, Machine(false, candy, coins)) =>
          Machine(true, candy - 1, coins)
      }
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (i => modify[Machine](processInput(i))))
      state <- get
    } yield (state.candies, state.coins)
}
