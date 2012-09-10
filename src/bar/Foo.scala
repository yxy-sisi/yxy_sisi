                                                                     
                                                                     
                                                                     
                                             
/**
 *Name: Xinya Ye  ID:1602367
 */

/**
 *Step1 an abstract base class for trees. define a class Tree with constructors Empty and mkTree.
 */
package bar

abstract class Tree
case class mkTree(x: Int, Left: Tree, Right: Tree) extends Tree
case object Empty extends Tree

def mkBST(x:Int,tree:Tree): Tree = {
t match {
case Empty => Nil
case mkTree(e,l,r) => if( e == x ) x :: mkBST(left) ::: mkBST (right)
mkBST(left) ::: mkBST(right)
}
}

/**
 Step 1.1 a Scala expression to contrust the binary tree t1 with 1 at the  root, and 2 and 3 as the left and right subtrees.
 */
val t1 = mkTree(1,mkTree(2,Empty,Empty),mkTree(3,Empty,Empty))



/**
 Step 1.2 count the number of elements in the tree.Yield the sum of numbers  found in the tree.
 */
 abstract class Tree
case class Node(value: Int, left: Tree, right: Tree) extends Tree
case object Nil extends Tree

object Tree {
  def fold[B](t: Tree, e: B, n: (Int, B, B) => B): B = t match {
    case Node(value, l, r) => n(value,fold(l,e,n),fold(r,e,n))
    case _ => e
  }
  def sumTree(t: Tree) = {
    val nodeValue: Tree=>Int = {
      case Node(v,_,_) => v
      case _ => 0
    }
    fold[Tree](t,Nil,(acc,l,r)=>Node(acc + nodeValue(l) + nodeValue(r) ,l,r)) 
  }
} 
 


 def sizeTree(t: Tree) = {
    val nodeValue: Tree=>Int = {
      case mkTree(v,_,_) => v
      case _ => 0
    }
    sizeTree.foldLeft(Empty)(_+_)

(=>mkTree(acc + nodeValue(l) + nodeValue(r) ,l,r)) 
  }
mkTree.foldLeft(e)
def mkBST(numberlist: List[Int])(implicit order: Ordering[Int]) = {
    numberlist.foldLeft[Tree](Empty)((tree, x) => insert(x, tree))
  }

/*
Step 1.3 display tree and yield it as printable tree
*/

def displayTree(t: Tree): Tree= {
t
 {
case mkTree(x,l,r)=> print(x); displayTree(left);displayTree(right)
case Empty => // do nothing}
}
}




/**
 2 find for and yield
 */
abstract class Tree
case class mkTree(x: Int, Left: Tree, Right: Tree) extends Tree
case object Empty extends Tree
def sizeTree(t:Tree,x:Int
:Tree = { 
// create a function called sizeTree to extract Tree value

  val size
}

abstract case class Tree
case class mkTree(x: Int, left: Tree, right: Tree) extends Tree
case object Empty extends Tree
object Tree {
  def fold[B](t: Tree, e: B, n: (Int, B, B) => B): B = t match {
    case Node(value, l, r) => n(value,fold(l,e,n),fold(r,e,n))
    case _ => e
  }
  def sumTree(t: Tree): Tree = 
    fold(t, Nil(), (a, b: Tree, c: Tree) => {
      val left = b match {
        case Node(value, _, _) => value
        case _ => 0
      }
      val right = c match {
        case Node(value, _, _) => value
        case _ => 0
      }
      Node(a+left+right,b,c)
    })
}



diplayTree(t: Tree): Unit={
t
 {
  case mkTree(x,l,r)=>print(x);displayTree(l);displayTree(r)
case Empty=>
}
}


/**
 *Step2 creat a tree that can insert Int x to Tree
 */
object Tree {
abstract class Tree
case class mkTree(x: Int, Left: Tree, Right: Tree) extends Tree

case object Empty extends Tree

def insert( x: Int, t: Tree): Tree ={ t match {

  case Empty => mkTree(x, Empty, Empty)

  case mkTree(e, l, r) =>

       if(x == e) t

      else if(x < e) mkTree(e, insert(x, l), r)


      else mkTree(e, l, insert(x, r))
 }
}
}
/**
 *test 哪错了？
 */
def insert(x:
def main(args: Array[String]) = {
val tree = mkTree(List(1,2,3))}


/*
step3
*/
;;;;3.1
abstract class Tree
case class mkTree(x: Int, Left: Tree, Right: Tree) extends Tree
case object Empty extends Tree

def mkBST(numberlist: List[Int])(implicit order: Ordering[Int]) = {
    numberlist.foldLeft[Tree](Empty)((tree, x) => 
insert(x, tree))
  }

/*
step 3.2 
*/
abstract class Tree
case class mkTree(x: Int, Left: Tree, Right: Tree) extends Tree
case object Empty extends Tree
def insert( x: Int, t: Tree): Tree ={ t match {

  case Empty => mkTree(x, Empty, Empty)

  case mkTree(e, l, r) =>

      if(x < e) mkTree(e, insert(x, l), r)
mkTree(e, l, insert(x, r))
 }
}
def mkBST(numberlist: List[Int])(implicit order: Ordering[Int]) = {
    numberlist.foldLeft[Tree](Empty)((tree, x) => insert(x, tree))
  }


/*
step3 testing function
*/
def main(args: Array[String]) {
    mkBST(List(7, 3, 5, 1, 9, 11))
  }

def main(args: Array[String]) {
    mkBinarySearchTree(List(7, 3, 5, 1, 9, 9, 11))
  }


step 4
abstract class Tree
case class mkTree(x: Int, Left: Tree, Right: Tree) extends Tree
case object Empty extends Tree
/*
 define a function for in-order traversals,':::'is used to  add a given element in the list infront of this list.
*/
def in_order(t: Tree): List[Int]= t
 {
case Empty =>List()
case mkTree(e,l,r) => in_order(l)::: List(e) ::: in_order(r)
}

def pre_order(t: Tree): List[Int]= t match {
case Empty =>List()
case mkTree(e,l,r) => List(e)::: pre_order(l) ::: pre_order(r)
}



//step5 不对 
abstract class Application
case object sort extends Application
case class Array

def sort(st:Array[Int]):Array[Int]={  
    if(st.length<=1)  
      st  
    else{  
      val p = st(st.length/2)  
      Array.concat(sort(st filter (p>)),  
                   st filter (p==),  
                   sort(st filter (p<)))  
    }  
  }  
