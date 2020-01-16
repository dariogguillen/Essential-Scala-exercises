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

  trait Shape {
    def sides: Int
    def perimeter: Double
    def area: Double
  }

  sealed trait Rectangular extends Shape {
    def width: Double
    def height: Double
    def sides: Int                 = 4
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
}
