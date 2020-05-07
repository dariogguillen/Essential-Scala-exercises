object Generics extends App {

  // 5.1.3.1 Generic List
  sealed trait LinkedList[A] {

    def length: Int = this match {
      case End()            => 0
      case Pair(head, tail) => 1 + tail.length
    }

    def contains(x: A): Boolean = this match {
      case End()            => false
      case Pair(head, tail) => if (head == x) true else tail.contains(x)
    }

    // def apply(x: Int): A = this match {
    // case End()            => throw new Exception("Attempted to get element from an Empty list")
    // case Pair(head, tail) => if (x == 0) head else tail(x - 1)
    // }

    def apply(x: Int): Result[A] = this match {
      case End()            => Failure("Index out of bounds")
      case Pair(head, tail) => if (x == 0) Success(head) else tail(x - 1)
    }
  }
  final case class End[A]() extends LinkedList[A]
  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

  sealed trait Result[A]
  case class Success[A](result: A) extends Result[A]
  case class Failure[A](reason: String) extends Result[A]

  // 5.1.3.2 Working with generic types
  val example = Pair(1, Pair(2, Pair(3, End())))
  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End().length == 0)

  assert(example.contains(3) == true)
  assert(example.contains(4) == false)
  assert(End().contains(0) == false)
  // This should not compile
  // example.contains("not an Int")

  // assert(example(0) == 1)
  // assert(example(1) == 2)
  // assert(example(2) == 3)
  // assert(try {
  // example(3)
  // false
  // } catch {
  // case e: Exception => true
  // })

  assert(example(0) == Success(1))
  assert(example(1) == Success(2))
  assert(example(2) == Success(3))
  assert(example(3) == Failure("Index out of bounds"))
}
