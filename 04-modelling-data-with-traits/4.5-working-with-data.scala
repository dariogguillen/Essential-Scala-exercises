object wwdata extends App {
  // 4.5.1 Structural Recursion using Polymorphism
  sealed trait A { def foo: String }
  final case class B() extends A { def foo: String = "It's B!" }
  final case class C() extends A { def foo: String = "It's C!" }

  val anA: A = B()
  println(anA.foo)
  val anotherA: A = C()
  println(anotherA.foo)

  // if an implementation is provided in the trait, the extended class must override it
  sealed trait aa { def foo: String                           = "It's a!" }
  final case class bb() extends aa { override def foo: String = "It's b!" }
  final case class cc() extends aa { override def foo: String = "It's c!" }

  val ana: aa = bb()
  println(ana.foo)
  val anothera: aa = cc()
  println(anothera.foo)

  // 4.5.3 A complete example
  // polymorphism
  sealed trait Feline { def dinner: Food }
  final case class Lion() extends Feline { def dinner: Food                    = Antelope }
  final case class Tiger() extends Feline { def dinner: Food                   = TigerFood }
  final case class Panther() extends Feline { def dinner: Food                 = Licorice }
  final case class Cat(favoriteFoos: String) extends Feline { def dinner: Food = CatFood(favoriteFoos) }

  // pattern matching
  sealed trait Felinee {

    def dinner: Food =
      this match {
        case Lionn()            => Antelope
        case Tigerr()           => TigerFood
        case Pantherr()         => Licorice
        case Catt(favoriteFoos) => CatFood(favoriteFoos)
      }
  }

  object Dinner {

    def dinner(feline: Felinee): Food =
      feline match {
        case Lionn()            => Antelope
        case Tigerr()           => TigerFood
        case Pantherr()         => Licorice
        case Catt(favoriteFoos) => CatFood(favoriteFoos)
      }
  }

  final case class Lionn() extends Felinee
  final case class Tigerr() extends Felinee
  final case class Pantherr() extends Felinee
  final case class Catt(favoriteFoos: String) extends Felinee

  sealed trait Food
  case object Antelope extends Food
  case object TigerFood extends Food
  case object Licorice extends Food
  final case class CatFood(food: String) extends Food

  // 4.5.6 Exercises
  // 4.5.6.1 TrafficLight
  sealed trait TrafficLight { def next: TrafficLight }
  final case object Red extends TrafficLight { def next: TrafficLight    = Green }
  final case object Green extends TrafficLight { def next: TrafficLight  = Yellow }
  final case object Yellow extends TrafficLight { def next: TrafficLight = Red }

  sealed trait TLight {

    def next: TLight = this match {
      case Tred    => Tgreen
      case Tgreen  => Tyellow
      case Tyellow => Tred
    }
  }

  final case object Tred extends TLight
  final case object Tgreen extends TLight
  final case object Tyellow extends TLight

  // 4.5.6.2 Calculator
  sealed trait Calculation
  final case class Success(result: Int) extends Calculation
  final case class Failure(reason: String) extends Calculation

  object Calculator {

    def +(cal: Calculation, x: Int): Calculation = cal match {
      case Success(result) => Success(result + x)
      case Failure(reason) => Failure(reason)
    }

    def -(cal: Calculation, x: Int): Calculation = cal match {
      case Success(result) => Success(result - x)
      case Failure(reason) => Failure(reason)
    }

    def /(cal: Calculation, x: Int): Calculation = cal match {
      case Failure(reason) => Failure(reason)
      case Success(result) => if (x == 0) Failure("Division by zero") else Success(result / x)
    }
  }
  assert(Calculator.+(Success(1), 1) == Success(2))
  assert(Calculator.-(Success(1), 1) == Success(0))
  assert(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))

  assert(Calculator./(Success(4), 2) == Success(2))
  assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
  assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))
}
