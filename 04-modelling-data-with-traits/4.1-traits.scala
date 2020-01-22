object traits extends App {
  // 4.1.4.1 Cats, and More Cats

  trait Feline {
    def color: String
    def sound: String
  }

  trait BigCat extends Feline {
    override def sound: String = "roar"
  }

  case class Cat(color: String, food: String) extends Feline {
    val sound: String = "meow"
  }

  case class Lion(color: String, maneSize: Int) extends BigCat

  case class Tiger(color: String) extends BigCat

  case class Panther(color: String) extends BigCat

  // 4.1.4.2 Shaping Up With Traits
  // 4.1.4.3 Shaping Up 2 (Da Streets)

  sealed trait Shape {
    def sides: Int
    def perimeter: Double
    def area: Double
  }

  sealed trait Rectangular extends Shape {
    def width: Double
    def height: Double
    def sides: Int = 4

    override def perimeter: Double = 2 * width + 2 * height
    override def area: Double      = width * height
  }

  case class Circle(radius: Double) extends Shape {
    val sides: Int        = 1
    val perimeter: Double = Math.PI * radius
    val area: Double      = Math.PI * radius * radius
  }

  case class Rectangle(width: Double, height: Double) extends Rectangular

  case class Square(side: Double) extends Rectangular {
    val width: Double  = side
    val height: Double = side
  }

  // 4.2.2.1 Printing Shapes
  object Draw {

    def apply(shape: Shape, color: Color) = {
      val currentColor = color match {
        case Pink(_, _, _)        => "Pink"
        case Red(_, _, _)         => "Red"
        case Yellow(_, _, _)      => "Yellow"
        case CustomColor(_, _, _) => color.isDark
      }
      shape match {
        case Circle(radius)           => println(s"A $currentColor circle of radius $radius cm")
        case Rectangle(width, height) => println(s"A $currentColor rectangle of width $width cm and height $height cm")
        case Square(side)             => println(s"A $currentColor square of side $side cm")
      }
    }
  }

  // 4.2.2.2 The Color and the Shape
  sealed trait Color {
    def red: Int
    def green: Int
    def blue: Int

    def isDark: String = if (red == 1 || green == 1) "Dark" else "light"
  }

  case class CustomColor(red: Int, green: Int, blue: Int) extends Color

  case class Red(red: Int = 1, green: Int = 0, blue: Int = 0) extends Color
  case class Yellow(green: Int = 1, red: Int = 0, blue: Int = 1) extends Color
  case class Pink(red: Int = 1, blue: Int = 1, green: Int = 0) extends Color

  Draw(Square(5), Pink())
  Draw(Circle(10), CustomColor(1, 1, 1))

  // 4.2.2.3 A Short Division Exercise
  sealed trait DivisionResult
  final case class Finite(num: Int) extends DivisionResult
  final case object Infinit extends DivisionResult

  object divide {
    def apply(x: Int, y: Int): DivisionResult = if (y == 0) Infinit else Finite(x / y)
  }

  divide(1, 0) match {
    case Finite(num) => s"It's finite: $num"
    case Infinit     => s"It's infinite"
  }
}
