import scala.List
import scala.Nil

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(a => Some(a)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if(f(a)) Some(a) else None)
}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object Option {
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def variance(xs: Seq[Double]): Option[Double] = {
    val mean =
      if (xs.length == 0)
        None
      else
        Some(xs.sum / xs.length)
    mean.map(
      x => xs.foldLeft(0.0)((b, a) => b + math.pow(a - x, 2)) / xs.length
    )
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)


  // def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
  //   a.foldLeft(None: Option[List[A]])((b, a) => (b, a) match {
  //     case (_, None) => None
  //     case (None, Some(aa)) => Some(List(aa))
  //     case (Some(bb), Some(aa)) => Some(bb ::: List(aa))
  //   })
  // }

  def sequence[A](l: List[Option[A]]): Option[List[A]] =
    traverse(l)(a => a)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldLeft(Some(Nil): Option[List[B]])((b, a) => (b, f(a)) match {
      case (_, None) => None
      case (None, _) => None
      case (Some(bb), Some(aa)) => Some(bb ::: List(aa))
    })
  }
}


