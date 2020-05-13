object genericTypes extends App {
  // 5.4.1.1 Exercise: Pairs
  case class Pair[A, B](one: A, two: B)

  val pair = Pair("hi", 2)
  println(pair.one)
  println(pair.two)

  // 5.4.3.1 Exercise: Generic Sum Type
  sealed trait Sum[A, B] {

    def fold[C](left: A => C, right: B => C): C = this match {
      case Left(value)  => left(value)
      case Right(value) => right(value)
    }
  }

  final case class Left[A, B](value: A) extends Sum[A, B]
  final case class Right[A, B](value: B) extends Sum[A, B]
  println(Left[Int, String](2).fold(a => a * 2, a => s"$a -"))
  println(Right[Int, String]("hi").fold(a => a * 2, a => s"$a -"))

  // 5.4.4.1 Exercise: Maybe that Was a Mistake
  sealed trait Maybe[A] {

    def fold[B](full: A => B, empty: B): B = this match {
      case Empty()     => empty
      case Full(value) => full(value)
    }

    def flatMap[B](fn: A => Maybe[B]): Maybe[B] = this match {
      case Empty()     => Empty[B]()
      case Full(value) => fn(value)
    }

    def map[B](f: A => B): Maybe[B] = this match {
      case Empty()     => Empty[B]()
      case Full(value) => Full(f(value))
    }

    def map0[B](f: A => B): Maybe[B] = flatMap[B](v => Full(f(v)))
  }
  final case class Full[A](value: A) extends Maybe[A]
  final case class Empty[A]() extends Maybe[A]

  def mightFail0: Maybe[Int] = Full(1)
  def mightFail1: Maybe[Int] = Full(2)
  def mightFail2: Maybe[Int] = Empty()

  val mightFail = mightFail0.flatMap { x =>
    mightFail1.flatMap { y =>
      // mightFail2.flatMap { z =>
      Full(x + y)
      // }
    }
  }

  println(mightFail)
}
