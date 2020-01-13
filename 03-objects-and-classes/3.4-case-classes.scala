object CaseClasses extends App {
  // 3.4.5.1 Case Cats
  case class Cat(colour: String, food: String)

  // 3.4.5.2 Roger Ebert Said it Best...

  case class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
    def name: String = s"$firstName $lastName"
  }

  object Director {
    def older(a: Director, b: Director): Director = if (a.yearOfBirth < a.yearOfBirth) a else b
  }

  class Film(val name: String, val yearOfRelease: Int, val imdbRating: Double, val director: Director) {
    def directorAge: Int                          = yearOfRelease - director.yearOfBirth
    def isDirectedBy(director: Director): Boolean = this.director == director

    def copy(
      name: String = this.name,
      yearOfRelease: Int = this.yearOfRelease,
      imdbRating: Double = this.imdbRating,
      director: Director = this.director
    ): Film = Film(name, yearOfRelease, imdbRating, director)
  }

  object Film {
    def highestRating(a: Film, b: Film): Film = if (a.imdbRating > b.imdbRating) a else b

    def oldestDirectorAtTheTime(a: Film, b: Film): Director =
      if (a.directorAge > b.directorAge) a.director else b.director
  }

  // Case Class Counter
  case class Counter(count: Int = 0) {
    def inc: Counter = copy(count + 1)
    def dec: Counter = copy(count - 1)
  }

  // Application, Application, Application
  case class Person(firstName: String, lastName: String) {
    def name: String = s"$firstName $lastName"
  }

  object Person {

    def apply(name: String): Person = {
      val parts = name.split(" ")
      new Person(parts(0), parts(1))
    }
  }
}
