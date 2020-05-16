object Variance extends App {
  sealed trait Maybe[+A]
  final case class Full[A](v: A) extends Maybe[A]
  final case object Empty extends Maybe[Nothing]

  val x: Maybe[Int]    = Empty
  val y: Maybe[String] = Empty

  sealed trait Sum[+A, +B] {

    def fold[C](failure: A => C, success: B => C): C = this match {
      case Failure(value) => failure(value)
      case Success(value) => success(value)
    }

    def map[C](f: B => C): Sum[A, C] = this match {
      case Failure(value) => Failure(value)
      case Success(value) => Success(f(value))
    }

    def flatMap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] = this match {
      case Failure(value) => Failure(value)
      case Success(value) => f(value)
    }
  }

  final case class Failure[A, Nothing](value: A) extends Sum[A, Nothing]
  final case class Success[Nothing, B](value: B) extends Sum[Nothing, B]

  // 5.6.6.2 Calculator Again

  sealed trait Expression {

    def eval: Sum[String, Double] = this match {
      case Addition(left, right)    => lift2(left, right, (l, r) => Success(l + r))
      case Subtraction(left, right) => lift2(left, right, (l, r) => Success(l - r))
      case Division(left, right) =>
        lift2(left, right, (l, r) => if (r == 0) Failure("Division by zero") else Success(l / r))
      case SquareRoot(value) =>
        value.eval.flatMap(v => {
          if (v < 0) Failure("Square root of negative number") else Success(Math.sqrt(v))
        })
      case Number(value) => Success(value)
    }

    def lift2(l: Expression, r: Expression, f: (Double, Double) => Sum[String, Double]) =
      l.eval.flatMap { left =>
        r.eval.flatMap { right =>
          f(left, right)
        }
      }
  }

  final case class Addition(left: Expression, right: Expression) extends Expression
  final case class Subtraction(left: Expression, right: Expression) extends Expression
  final case class Division(left: Expression, right: Expression) extends Expression
  final case class SquareRoot(value: Expression) extends Expression
  final case class Number(value: Double) extends Expression

  assert(Addition(Number(1), Number(2)).eval == Success(3))
  assert(SquareRoot(Number(-1)).eval == Failure("Square root of negative number"))
  assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))
  assert(Division(Addition(Subtraction(Number(8), Number(6)), Number(2)), Number(2)).eval == Success(2.0))

  ///////////////
  trait Visitor {
    def name: String
  }

  case class Visitor1(name: String) extends Visitor
  case class Visitor2(name: String) extends Visitor
  // case class Visitor3(name: String)

  case class WebAnalytics[A <: Visitor](
    visitor: A,
    pageViews: Int
  )

  val susi = Visitor1("Susi")
  val juan = Visitor2("Juan")
  // val dario = Visitor3("Dario")
  val wa1 = WebAnalytics(susi, 1)
  val wa2 = WebAnalytics(juan, 1)
  // val wa3   = WebAnalytics(dario, 1)
  println(wa1.visitor.name)
  println(wa2.visitor.name)
}
