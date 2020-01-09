object CompanionObject extends App {

  // 3.3.2.1 Friendly Preson Factory
  class Person(val firstName: String, val lastName: String) {
    def name: String = s"$firstName $lastName"
  }

  object Person {

    def apply(name: String): Person = {
      val parts = name.split(" ")
      new Person(parts(0), parts(1))
    }
  }

  val person = Person("John Doe") // Person.apply("John Doe")
  println(person.firstName)
  println(person.lastName)
}
