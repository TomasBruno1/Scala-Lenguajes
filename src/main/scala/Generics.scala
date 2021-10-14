object Generics extends App:

  sealed abstract class Tree[+T] // El "+" permite guardar el tipo de dato a uno de mayor jerarquia (*)

  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

  case class Leaf[T](x: T) extends Tree[T]

  val tree: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(4)))
  val tree2: Tree[Any] = tree // (*)

  val list: List[Int] = List(1,2,3)
  val anyList: List[Any] = list

  val s: java.lang.String = "12345"
  s.filter(_ != '4')

  println(tree)