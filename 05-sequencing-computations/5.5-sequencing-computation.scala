object sequencingComputation extends App {

  sealed trait LinkedList[A] {

    def fold[B](end: B, f: (A, B) => B): B = this match {
      case End()            => end
      case Pair(head, tail) => f(head, tail.fold(end, f))
    }

    // 5.5.1 Map
    def map[B](fn: A => B): LinkedList[B] = this match {
      case End()            => End[B]()
      case Pair(head, tail) => Pair(fn(head), tail.map(fn))
    }
  }
  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
  final case class End[A]() extends LinkedList[A]

  // 5.5.4 Exercises
  // 5.5.4.1 Mapping Lists
  val list: LinkedList[Int] = Pair(1, Pair(2, Pair(3, End())))

  val doubleList: LinkedList[Int] = list.map(_ * 2)
  println(doubleList)

  val plusOneList: LinkedList[Int] = list.map(_ + 1)
  println(plusOneList)

  val dividedList: LinkedList[Int] = list.map(_ / 3)
  println(dividedList)

  // 5.5.4.3 Sequencing Coputations
  val list0  = List(1, 2, 3)
  val list00 = list0.flatMap(x => List(x, -x))
  println(list00)

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

  val list1: List[Maybe[Int]] = List(Full(3), Full(2), Full(1))

  val list10: List[Maybe[Int]] = list1.map(
    maybe =>
      maybe.flatMap[Int] { x =>
        if (x % 2 == 0) Full(x) else Empty()
      }
  )

  println(list10)

  sealed trait Sum[A, B] {

    def fold[C](failure: A => C, success: B => C): C = this match {
      case Failure(value) => failure(value)
      case Success(value) => success(value)
    }

    def map[C](f: B => C): Sum[A, C] = this match {
      case Failure(value) => Failure(value)
      case Success(value) => Success(f(value))
    }

    def flatMap[C](f: B => Sum[A, C]): Sum[A, C] = this match {
      case Failure(value) => Failure(value)
      case Success(value) => f(value)
    }
  }

  final case class Failure[A, B](value: A) extends Sum[A, B]
  final case class Success[A, B](value: B) extends Sum[A, B]

  println(Failure[Int, String](3).map(_ + 1))
  println(Success[String, Int](3).map(_ + 1))
  println(Failure[Int, String](3).flatMap(x => Success(x + 1)))
  println(Success[String, Int](3).flatMap(x => Success(x + 1)))
}
