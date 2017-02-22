
object Tree {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  // exercise 3.25
  def size[A](root : Tree[A]) : Int = root match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  val tree7 = new Branch(
                new Branch(
                  new Leaf(8),
                  new Branch(
                    new Leaf(2),
                    new Leaf(12)
                  )
                ),
                new Branch(
                  new Leaf(3),
                  new Leaf(24)
                )
              )

  val size7 = size(tree7)

  // exercise 3.26
  def maximum(t: Tree[Int]) : Int = {

    def maximumInner(t1: Tree[Int], acc: Int) : Int = t1 match {
      case Leaf(value) => Math.max(value, acc)
      case Branch(left, right) => maximumInner(left, acc) max maximumInner(right, acc)
    }
    maximumInner(t, 0)
  }
  maximum(tree7)

  // exercise 3.27

  def depth[A](t: Tree[A]) : Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  val depth3 : Int = depth(tree7)

  // exercise 3.28

  def map[A,B](tr: Tree[A])(f: A => B) : Tree[B] = tr match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  val treeAdd1 = map(tree7)((a: Int) => a + 1)

  // exercise 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B) : B = t match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeByFold[A](root : Tree[A]) : Int = {
    fold(root)((a: A) => 1)((b: Int, c: Int) => 1 + b + c)
  }

  val sizeByFoldVal = sizeByFold(tree7)

  def maximumByFold(t: Tree[Int]) : Int = {
    fold(t)((a: Int) => a)(_ max _)
  }

  val maximumByFoldVal = maximumByFold(tree7)

  def depthByFold[A](t: Tree[A]) : Int = {
    fold(t)((a: A) => 0)((b: Int, c: Int) => 1 + (b max c))
  }

  val depthByFoldVal = depthByFold(tree7)

  def mapByFold[A,B](tr: Tree[A])(f: A => B) : Tree[B] = {
    fold(tr)((v: A) => Leaf(f(v)): Tree[B])((a: Tree[B], b: Tree[B]) => Branch(a, b))
  }

  val treeAdd2 = mapByFold(tree7)((a: Int) => a + 2)
}