import scala.annotation.tailrec

object recursiveData extends App {

  sealed trait IntList {

    // 4.6.3.1 A list of Methods
    def length: Int = this match {
      case End              => 0
      case Pair(head, tail) => 1 + tail.length
    }

    def product: Int = this match {
      case End              => 1
      case Pair(head, tail) => head * tail.product
    }

    def double: IntList = this match {
      case End              => End
      case Pair(head, tail) => Pair(head * 2, tail.double)
    }
  }
  case object End extends IntList
  final case class Pair(head: Int, tail: IntList) extends IntList

  Pair(1, Pair(2, Pair(3, End)))

  def sum0(list: IntList): Int = list match {
    case End              => 0
    case Pair(head, tail) => head + sum0(tail)
  }

  @tailrec
  def sum(list: IntList, acc: Int = 0): Int = list match {
    case End              => acc
    case Pair(head, tail) => sum(tail, acc + head)
  }

  val example = Pair(1, Pair(2, Pair(3, End)))
  assert(sum(example) == 6)
  assert(sum(example.tail) == 5)
  assert(sum(End) == 0)
  assert(sum0(example) == 6)
  assert(sum0(example.tail) == 5)
  assert(sum0(End) == 0)

  // 4.6.3.1 A list of Methods
  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End.length == 0)

  assert(example.product == 6)
  assert(example.tail.product == 6)
  assert(End.product == 1)

  assert(example.double == Pair(2, Pair(4, Pair(6, End))))
  assert(example.tail.double == Pair(4, Pair(6, End)))
  assert(End.double == End)

  // 4.6.3.2 The Forest of Trees
  sealed trait Tree {
    def sum1: Int
    def double1: Tree
  }

  final case class Node(l: Tree, r: Tree) extends Tree {
    def sum1: Int     = l.sum1 + r.sum1
    def double1: Tree = Node(l.double1, r.double1)
  }

  final case class Leaf(el: Int) extends Tree {
    def sum1: Int     = el
    def double1: Tree = Leaf(el * 2)
  }

  object TreeOps {

    def sum0(tree: Tree): Int = tree match {
      case Leaf(el)   => el
      case Node(l, r) => sum0(l) + sum0(r)
    }

    def double0(tree: Tree): Tree = tree match {
      case Leaf(el)   => Leaf(el * 2)
      case Node(l, r) => Node(double0(l), double0(r))
    }
  }
}
