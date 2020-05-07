object Fold extends App {

  sealed trait LinkedList[A] {

    def fold[B](end: B, f: (A, B) => B): B = this match {
      case End()            => end
      case Pair(head, tail) => f(head, tail.fold(end, f))
    }

    // 5.5.1 Map
    def map[B](fn: A => B): LinkedList[B] = this match {
      case End()            => End[B]()
      case Pair(head, tail) => Pair(fn(head), tail.map(fn))
    }
  }
  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
  final case class End[A]() extends LinkedList[A]

  val list: LinkedList[(String, Int)] = Pair("a" -> 1, Pair("b" -> 2, End[(String, Int)]))

  val l0 = list.fold[(Int, String)](0 -> "", (h, t) => {
    (h._2 + t._1) -> (h._1 + t._2)
  })

  val l1 = list.fold[LinkedList[(Int, String)]](End[(Int, String)], (h, t) => {
    Pair[(Int, String)](h._2 -> h._1, t)
  })

  println(l1)

  println(l0)

  // 5.3.4.1 Tree

  sealed trait Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B): B
  }

  final case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B): B = node(l.fold(node, leaf), r.fold(node, leaf))
  }

  final case class Leaf[A](el: A) extends Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B): B = leaf(el)
  }

  val tree: Tree[String] = Node(
    Node(Leaf("To"), Leaf("iterate")),
    Node(
      Node(Leaf("is"), Leaf("human,")),
      Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))
    )
  )

  val res = tree.fold[String]((a, b) => s"$a $b", str => str)
  println(res)
}
