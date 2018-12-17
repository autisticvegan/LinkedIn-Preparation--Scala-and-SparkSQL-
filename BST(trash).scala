
import scala.collection.mutable._
import scala.collection //???

object p {
  def lastB[A](ls: List[A]): A = ls.last
  
  def lastR[A](ls: List[A]): A = ls match {
    
    case h :: Nil => h
    case _ :: tail => lastR(tail)
    case _ => throw new NoSuchElementException
    
  }
  
}

//need comp because searching for equality
class BST[K,V](comp:(K,K) => Int) extends scala.collection.mutable.Map[K,V] {
  
  class Node(var Key:K, var Value:V)
  {
    var left:Node = null
    var right:Node = null
  }
  
  private var root:Node = null
  
  def +=(kv: (K,V)) =
  {
    val (key,value) = kv //dont want to have to type _
    def recur(n:Node):Node =
    {
        if(n == null) new Node(key,value)
        else
        {
          val c = comp(key,n.Key)
          if(c == 0)
          {
            n.Value = value
          }
          else if(c < 0)
          {
            n.left = recur(n.left)
          }
          else
          {
            n.right = recur(n.right)
          }
          n
        }
    }
root = recur(root)
      this
  }
  
  def -=(key: K) = 
  {
     this
  }
  
  def iterator = new Iterator[(K,V)]
{
    val stack = new ArrayStack[Node]
    pushAllLeft(root)
    
    def next:(K,V) = 
    {
      val ret = stack.pop()
      pushAllLeft(ret.right)
      ret.Key -> ret.Value
    }
    
    def hasNext = !stack.isEmpty
    
    def pushAllLeft(n:Node)
    {
      if(n!=null)
      {
        stack.push(n)
        pushAllLeft(n.left)
      }
    }
}
  
  private def inorder(visit: V => Unit)
  {
    def recur(n:Node)
    {
      if(n!=null)
      {
        recur(n.left)
        visit(n.Value)
        recur(n.right)
      }
    }
    recur(root)
  }
  
  
  def get(key: K): Option[V] = 
  {
    var searcher = root
    
    while(searcher != null && searcher.Key != key)
    {
      searcher = if(comp(key,searcher.Key) < 0) searcher.left else searcher.right
    }
    
    if(searcher == null) None
    else Some(searcher.Value)
    
  }
  
  
}