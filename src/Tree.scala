import scala.math._
object Tree {

  def insert[T](x: T, tree: Tree[T])(implicit order: Ordering[T]): Tree[T] = {
    tree match {
      case Empty => Node(x, Empty, Empty)
      case Node(v, left, right) if order.gt(x, v) => Node(v, left, insert(x, right))
      case Node(v, left, right) if order.lt(x, v) => Node(v, insert(x, left), right)
      case Node(v, left, right) if order.equiv(x, v) => Node(v, left, right) //其实没有必要重新创建
    }
  }

  def mkBinarySearchTree[T](list: List[T])(implicit order: Ordering[T]) = {
    list.foldLeft[Tree[T]](Empty)((tree, value) => insert(value, tree))
  }

  def main(args: Array[String]) {}

}

trait Tree[+T] {
  def dfs(func: T => Unit) {
    this match {
      case Empty =>
      case Node(value, left, right) => func(value); left dfs func; right dfs func
    }
  }
}

/**
 * 空树
 */
case object Empty extends Tree[Nothing]
/**
 * 节点, 单个节点是一棵树
 */
case class Node[T](val value: T, val left: Tree[T], val right: Tree[T]) extends Tree[T]
/**
 * 叶子，只是为了创建更方便
 */
case class Leaf[T](override val value: T) extends Node[T](value, Empty, Empty)