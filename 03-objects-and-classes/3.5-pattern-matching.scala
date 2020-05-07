object PatternMaching extends App {
  case class Cat(name: String, colour: String, food: String)

  // 3.5.3.1 Feed the Cats

  object chipShop {

    def willServe(cat: Cat): Boolean = cat match {
      case Cat(_, _, "chips") => true
      case _                  => false
    }
  }
  println(chipShop.willServe(Cat("cat0", "gray", "chips")))
  println(chipShop.willServe(Cat("cat0", "black", "Chips")))
}
