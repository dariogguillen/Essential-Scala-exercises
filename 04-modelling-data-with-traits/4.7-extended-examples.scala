object ExtendedExamples extends App {

  sealed trait Expression {

    def eval: Calculation =
      this match {
        case Addition(l, r) =>
          l.eval match {
            case Failure(reason) => Failure(reason)
            case Success(l0) =>
              r.eval match {
                case Failure(reason) => Failure(reason)
                case Success(r0)     => Success(l0 + r0)
              }
          }
        case Subtraction(l, r) =>
          l.eval match {
            case Failure(reason) => Failure(reason)
            case Success(l0) =>
              r.eval match {
                case Failure(reason) => Failure(reason)
                case Success(r0)     => Success(l0 - r0)
              }
          }
        case Division(l, r) =>
          l.eval match {
            case Failure(reason) => Failure(reason)
            case Success(l0) =>
              r.eval match {
                case Failure(reason) => Failure(reason)
                case Success(r0) =>
                  if (r0 == 0)
                    Failure("Division by zero")
                  else
                    Success(l0 / r0)
              }
          }
        case SquareRoot(v) =>
          v.eval match {
            case Success(r) =>
              if (r < 0)
                Failure("Square root of negative number")
              else
                Success(Math.sqrt(r))
            case Failure(reason) => Failure(reason)
          }
        case Number(v) => Success(v)
      }
  }

  final case class Addition(l: Expression, r: Expression) extends Expression
  final case class Subtraction(l: Expression, r: Expression) extends Expression
  final case class Division(l: Expression, r: Expression) extends Expression
  final case class SquareRoot(v: Expression) extends Expression
  final case class Number(v: Double) extends Expression

  sealed trait Calculation
  final case class Success(result: Double) extends Calculation
  final case class Failure(result: String) extends Calculation

  assert(
    Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval ==
    Failure("Square root of negative number")
  )
  assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Success(4.0))
  assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))

  // 4.7.0.2 JSON
  sealed trait Json {

    def seqToString(s: SeqCell): String = s match {
      case SeqCell(h, t @ SeqCell(_, _)) => s"${h.print}, ${seqToString(t)}"
      case SeqCell(h, SeqEnd)            => h.print
    }

    def objToString(o: ObjectCell): String = o match {
      case ObjectCell(k, v, t @ ObjectCell(_, _, _)) => s""""$k": ${v.print}, ${objToString(t)}"""
      case ObjectCell(k, v, ObjectEnd)               => s""""$k": ${v.print}"""
    }

    def print: String = this match {
      case JsNumber(value)         => s"$value"
      case JsString(value)         => s""""$value""""
      case JsBoolean(value)        => s"$value"
      case s @ SeqCell(_, _)       => s"[ ${seqToString(s)} ]"
      case o @ ObjectCell(_, _, _) => s"{ ${objToString(o)} }"
      case JsNull                  => "null"
      case _                       => ""
    }
  }
  final case class JsNumber(value: Double) extends Json
  final case class JsString(value: String) extends Json
  final case class JsBoolean(value: Boolean) extends Json

  case object JsNull extends Json

  sealed trait JsSequence extends Json
  final case class SeqCell(head: Json, tail: JsSequence) extends JsSequence
  case object SeqEnd extends JsSequence

  sealed trait JsObject extends Json
  final case class ObjectCell(key: String, value: Json, tail: JsObject) extends JsObject
  case object ObjectEnd extends JsObject

  println(SeqCell(JsString("a string"), SeqCell(JsNumber(1.0), SeqCell(JsBoolean(true), SeqEnd))).print)
  println(
    ObjectCell(
      "a",
      SeqCell(JsNumber(1.0), SeqCell(JsNumber(2.0), SeqCell(JsNumber(3.0), SeqEnd))),
      ObjectCell(
        "b",
        SeqCell(JsString("a"), SeqCell(JsString("b"), SeqCell(JsString("c"), SeqEnd))),
        ObjectCell(
          "c",
          ObjectCell(
            "doh",
            JsBoolean(true),
            ObjectCell("ray", JsBoolean(false), ObjectCell("me", JsNumber(1.0), ObjectEnd))
          ),
          ObjectEnd
        )
      )
    ).print
  )
}
