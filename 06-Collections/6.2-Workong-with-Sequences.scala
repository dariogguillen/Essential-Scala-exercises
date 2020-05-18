object WorkingWithSequences extends App {
  case class Film(name: String, yearOfRelease: Int, imdbRating: Double)

  case class Director(firstName: String, lastName: String, yearOfBirth: Int, films: Seq[Film])

  val memento    = new Film("Memento", 2000, 8.5)
  val darkKnight = new Film("Dark Knight", 2008, 9.0)
  val inception  = new Film("Inception", 2010, 8.8)

  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales  = new Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven        = new Film("Unforgiven", 1992, 8.3)
  val granTorino        = new Film("Gran Torino", 2008, 8.2)
  val invictus          = new Film("Invictus", 2009, 7.4)

  val predator          = new Film("Predator", 1987, 7.9)
  val dieHard           = new Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood =
    new Director("Clint", "Eastwood", 1930, Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))

  val mcTiernan = new Director("John", "McTiernan", 1951, Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))

  val nolan = new Director("Christopher", "Nolan", 1970, Seq(memento, darkKnight, inception))

  val someGuy = new Director("Just", "Some Guy", 1990, Seq())

  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)
  // 6.2.7.1 Heroes of the Silver Screen
  val nolanFilms: Seq[String] = nolan.films.map(_.name)

  val cinephile: Seq[String] = directors.flatMap(_.films.map(_.name))

  val vintageMcTiernan = mcTiernan.films.foldRight(Int.MaxValue)((x, y) => math.min(x.yearOfRelease, y))

  val highScoreTable  = directors.map(_.films.sortWith((a, b) => a.imdbRating > b.imdbRating)).flatten
  val highScoreTable0 = directors.flatMap(_.films).sortWith((a, b) => a.imdbRating > b.imdbRating)
  val highScoreTable1 = highScoreTable0.foldLeft(0d)((a, b) => a + b.imdbRating)
  directors.foreach(d => d.films.foreach(f => println(s"Tonight only! ${f.name} by ${d.firstName} ${d.lastName}")))
  val fromTheArchives = directors.map(_.films.sortWith((a, b) => a.yearOfRelease > b.yearOfRelease).headOption)

  def minimun(a: Seq[Int]): Int = a.foldLeft(Int.MaxValue)(math.min)

  def unique(a: Seq[Int]): Seq[Int] = a.foldLeft(Seq[Int]())((l, i) => if (l contains i) l else l.:+(i))

  def reverse[A](a: Seq[A]): Seq[A] = a.foldLeft(Seq.empty[A])((l, i) => l.+:(i))

  def map[A, B](a: Seq[A], f: A => B): Seq[B] = a.foldRight(Seq.empty[B])((i, l) => l.+:(f(i)))

  println(map(Seq(1, 2, 3, 4, 5), (x: Int) => x + 1))
}
