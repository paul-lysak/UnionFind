package util
import UnionFind._
import scala.Option
import collection.LinearSeq
;

/**
 * @author Paul Lysak
 * Date: 17.12.12
 * Time: 20:57
 */
class UnionFind(val nodesTree: UnionFind.AbstractNode, val size: Int, val groupsCount: Int) {
  def this(size: Int) = {
    this(buildTree(size), size, size)
  }
  private val pathCache: Array[List[Boolean]] = new Array[List[Boolean]](size)


  def simpleJoin(item1: Int, item2: Int): UnionFind = {
    join(item1, item2)._1
  }

  def join(item1: Int, item2: Int): (UnionFind, GroupNode, Boolean) = {
    val gr1 = find(item1)
    val gr2 = find(item2)
    if (gr1 == gr2) (this, gr1, false)
    else {
      val path1 = getPath(gr1.id)
      val path2 = getPath(gr2.id)
      val (tree, group) = (gr1.depth - gr2.depth).signum match {
        case -1 => {val group = GroupNode(gr2.id, gr1.groupSize+gr2.groupSize, gr2.depth)
          val tree = nodesTree.replaceByPath(path1, SingleNode(gr1.id, gr2.id)).replaceByPath(path2, group)
          (tree, group)
        }
        case 0 => {val group = GroupNode(gr1.id, gr1.groupSize + gr2.groupSize, gr1.depth+1)
          val tree = nodesTree.replaceByPath(path1, group).replaceByPath(path2, SingleNode(gr2.id, gr1.id))
          (tree, group)
        }
        case 1 => {
          val group = GroupNode(gr1.id, gr1.groupSize + gr2.groupSize, gr1.depth)
          val tree = nodesTree.replaceByPath(path1, group).replaceByPath(path2, SingleNode(gr2.id, gr1.id))
          (tree, group)
        }
      }
      (new UnionFind(tree, size, groupsCount - 1), group, true)
    }
  }

  def find(item: Int): GroupNode = {
    val path = getPath(item)
    val node = UnionFind.getByPath(nodesTree, path);
//    GroupNode(0, 1, 1)
    node match {
      case SingleNode(_, groupId) => if (item != groupId) find(groupId) else throw new Exception("SingleNode references itself: "+node)
      case group @ GroupNode(_, _, _) => group
      case _ => throw new Exception("Corrupted tree: "+nodesTree);
    }
  }


  def getPath(itemId: Int): List[Boolean] = {
    def buildBinList(acc: List[Boolean], x: Int): List[Boolean] = {
      if (x == 0) acc
      else buildBinList((x % 2 == 1) :: acc, x / 2)
    }
    //TODO there should be more elegant way then two reverses
    if (pathCache(itemId) != null)
      pathCache(itemId)
    else {
      pathCache.update(itemId, (buildBinList(Nil, itemId).reverse padTo(getDepth(size), false)).reverse)
      pathCache(itemId)
    }
  }

  def getGroups: Map[UnionFind.GroupNode, List[Int]] = {
    val groups = scala.collection.mutable.HashMap[GroupNode, List[Int]]()
    nodesTree foreach {leafNode: LeafNode => leafNode match {
      case group @ GroupNode(groupId, _, _) => {
        val groupMembers = groups.get(group) match {
          case Some(list) => list
          case None => List(groupId)
        }
        groups.put(group, groupMembers)
      }
      case SingleNode(id, groupId) => {
        val group = find(groupId)
        val groupMembers = (groups.get(group) match {
          case Some(list) => id :: list
          case None => List(id, group.id)})
        groups.put(group, groupMembers)}
      case EmptyNode() => {}
    }}
    groups.toMap
  }

}


object UnionFind {
  def buildTree(size: Int): AbstractNode = {
    buildTree(0, size, getDepth(size))._1
  }

  def getDepth(size: Int) = {
    var depth = 0;
    var subSize = size-1;
    while (subSize > 0) {
      depth += 1;
      subSize = subSize >> 1;
    }
    depth;
  }

  def buildTree(startIndex: Int, size: Int, depth: Int): (AbstractNode, Int) = {
    if (depth > 0 && size > 1) {
      val (leftTree, leftSize) = buildTree(startIndex, size, depth - 1)
      val (rightTree, rightSize) = buildTree(startIndex + leftSize, size - leftSize, depth - 1)
      (ChoiceNode(leftTree, rightTree), leftSize + rightSize)
    } else {
      if (size > 0) (GroupNode(startIndex, 1, 1), 1)
      else (EmptyNode(), 0)
    }
  }

  def getByPath(node: AbstractNode, path: List[Boolean]): AbstractNode = {
      if (path.isEmpty)
        node
      else
        node match {
          case ChoiceNode(left, right) => if (path.head) getByPath(right, path.tail) else getByPath(left, path.tail)
          case LeafNode() => node
        }
  }

  abstract case class AbstractNode() {
    def foreach(f: LeafNode => Unit): Unit

    def replaceByPath(path: List[Boolean], newNode: AbstractNode) = {
      def buildSubTree(tree: AbstractNode, path: List[Boolean], newNode: AbstractNode): AbstractNode = {
        tree match {
          case ChoiceNode(left, right) =>
            if (path.head) {
              val newRight = buildSubTree(right, path.tail, newNode)
              ChoiceNode(left, newRight)
            } else {
              val newLeft = buildSubTree(left, path.tail, newNode)
              ChoiceNode(newLeft, right)
            }
          case LeafNode() => {
            newNode
          } //may be some remaining path - ignore it
        }
      }
      buildSubTree(this, path, newNode);
    }
  }

  case class ChoiceNode(left: AbstractNode, right: AbstractNode) extends AbstractNode {
    def foreach(f: LeafNode => Unit): Unit = {
      left.foreach(f)
      right.foreach(f)
    }
  }
  case class LeafNode() extends AbstractNode {
    def foreach(f: LeafNode => Unit): Unit = {
      f(this)
    }
  }
  case class SingleNode(id: Int, groupId: Int) extends LeafNode
  case class GroupNode(id: Int, groupSize: Int, depth: Int) extends LeafNode
  case class EmptyNode() extends LeafNode //TODO do we need it?
}