

object tree extends App {
  


sealed abstract class Tree
case class Node(left: Tree, right: Tree) extends Tree
case class Leaf[A](value: A) extends Tree
case object EmptyLeaf extends Tree

// DSL-like assignment:
val treeA = Node(EmptyLeaf, Leaf(5))
val treeB = Node(Node(Leaf(2), Leaf(3)), Leaf(5))

// On Scala 2.8, modification through cloning:
val treeC = treeA.copy(left = treeB.left)

// Pretty printing:
println("Tree A: "+treeA)
println("Tree B: "+treeB)
println("Tree C: "+treeC)

// Comparison:
println("Tree A == Tree B: %s" format (treeA == treeB).toString)
println("Tree B == Tree C: %s" format (treeB == treeC).toString)

// Pattern matching:
treeB match {
  case Node(EmptyLeaf, right) => println("Can be reduced to "+right)
  case Node(left, EmptyLeaf) => println("Can be reduced to "+left)
  case _ => println(treeA+" cannot be reduced")
}



// Pattern matches can be safely done, because the compiler warns about
// non-exaustive matches:
def checkTree(t: Tree) = t match {
  case Node(EmptyLeaf, Node(left, right)) =>
  case Node(EmptyLeaf, Leaf(el)) => println("WOO")
  case Node(Node(left, right), EmptyLeaf) =>
  case Node(Leaf(el), EmptyLeaf) =>
  case Node(Node(l1, r1), Node(l2, r2)) =>
  case Node(Leaf(e1), Leaf(e2)) =>
  case Node(Node(left, right), Leaf(el)) =>
  case Node(Leaf(el), Node(left, right)) =>
  // case Node(EmptyLeaf, EmptyLeaf) =>
  case Leaf(el) =>
  case EmptyLeaf =>
}

checkTree(treeA)

}