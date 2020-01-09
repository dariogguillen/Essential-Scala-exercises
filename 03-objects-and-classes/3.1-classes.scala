object CatsAgain extends App {
  // 3.1.6.1 Cats, Again
  class Cat(val name: String, val colour: String, val food: String)

  val cat0 = new Cat("Oswald", "Black", "Milk")
  val cat1 = new Cat("Henderson", "Ginger", "Chips")
  val cat2 = new Cat("Quentin", "Tabby and white", "Curry")
  println(cat0)
  println(cat1)
  println(cat2)

  // Cats on the Prowl
  object ChipShop {
    def willServe(cat: Cat): Boolean = if (cat.food == "Chips") true else false
  }
  println(ChipShop.willServe(cat0))
  println(ChipShop.willServe(cat1))
  println(ChipShop.willServe(cat2))

  // 3.1.6.3 Directorial Debut
  class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
    def name: String = s"$firstName $lastName"

    def copy(
      firstName: String = this.firstName,
      lastName: String = this.lastName,
      yearOfBirth: Int = this.yearOfBirth
    ) = new Director(firstName, lastName, yearOfBirth)
  }
  val eastwood  = new Director("Clint", "Eastwood", 1930)
  val mcTiernan = new Director("John", "McTiernan", 1951)
  val nolan     = new Director("Christopher", "Nolan", 1970)
  val someBody  = new Director("Just", "Some Body", 1990)

  class Film(val name: String, val yearOfRelease: Int, val imdbRating: Double, val director: Director) {
    def directorAge: Int                          = yearOfRelease - director.yearOfBirth
    def isDirectedBy(director: Director): Boolean = this.director == director

    def copy(
      name: String = this.name,
      yearOfRelease: Int = this.yearOfRelease,
      imdbRating: Double = this.imdbRating,
      director: Director = this.director
    ): Film =
      new Film(name, yearOfRelease, imdbRating, director)
  }
  val memento    = new Film("Memento", 2000, 8.5, nolan)
  val darkKnight = new Film("Dark Knight", 2008, 9.0, nolan)
  val inception  = new Film("Inception", 2010, 8.8, nolan)

  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7, eastwood)
  val outlawJoseyWales  = new Film("The Outlaw Josey Wales", 1976, 7.9, eastwood)
  val unforgiven        = new Film("Unforgiven", 1992, 8.3, eastwood)
  val granTorino        = new Film("Gran Torino", 2008, 8.2, eastwood)
  val invictus          = new Film("Invictus", 2009, 7.4, eastwood)

  val predator          = new Film("Predator", 1987, 7.9, mcTiernan)
  val dieHard           = new Film("Die Hard", 1988, 8.3, mcTiernan)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6, mcTiernan)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8, mcTiernan)

  println(eastwood.yearOfBirth)
  println(memento.directorAge)
  println(dieHard.director.name)
  println(invictus.isDirectedBy(nolan))
  println(invictus.isDirectedBy(eastwood))
  println(highPlainsDrifter.copy(name = "L'homme des hautes plaines").name)
  println(
    thomasCrownAffair.copy(yearOfRelease = 1968, director = new Director("Norman", "Jewison", 1926)).director.name
  )
  println(inception.copy().copy().copy().yearOfRelease)

  // 3.1.6.4 Simple Counter
  class Counter(val count: Int = 0) {
    def inc: Counter = new Counter(count + 1)
    def dec: Counter = new Counter(count - 1)
  }
  val count = new Counter(10)
  println(count.inc.dec.inc.inc.count)

  // 3.1.6.5 Counting Faster
  class Counter0(val count: Int = 0) {
    def inc: Counter0 = inc()
    def dec: Counter0 = dec()

    def inc(x: Int = 1): Counter0 = new Counter0(count + x)
    def dec(x: Int = 1): Counter0 = new Counter0(count - x)
  }
  println(new Counter0(10).inc.dec(2).count)

  // 3.1.6.6 Additional Counting
  class Counter1(val count: Int = 0) {
    def inc: Counter0 = inc()
    def dec: Counter0 = dec()

    def inc(x: Int = 1): Counter0 = new Counter0(count + x)
    def dec(x: Int = 1): Counter0 = new Counter0(count - x)

    def adjust(x: Adder): Counter = new Counter(x.add(count))
  }

  class Adder(val amount: Int) {
    def add(in: Int): Int = in + amount
  }
  println(new Counter1(10).adjust(new Adder(5)).count)

  // 3.3.2.2 Extended Body of Work
  object Director {
    def apply(f: String, l: String, y: Int): Director = new Director(f, l, y)
    def older(a: Director, b: Director): Director     = if (a.yearOfBirth < a.yearOfBirth) a else b
  }

  object Film {
    def apply(n: String, y: Int, r: Double, d: Director): Film = new Film(n, y, r, d)
    def highestRating(a: Film, b: Film): Film                  = if (a.imdbRating > b.imdbRating) a else b

    def oldestDirectorAtTheTime(a: Film, b: Film): Director =
      if (a.directorAge > b.directorAge) a.director else b.director
  }
}
