import org.junit.runner.RunWith
import org.scalatest.{GivenWhenThen, FunSuite}
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import util.UnionFind
;
import util.UnionFind._;
;

/**
 * @author Paul Lysak
 *         Date: 17.12.12
 *         Time: 20:57
 */
@RunWith(classOf[JUnitRunner])
class UnionFindSuite extends FunSuite with ShouldMatchers with GivenWhenThen {
  val uf3it3gr = new UnionFind(ChoiceNode(ChoiceNode(GroupNode(0, 1, 1), GroupNode(1, 1, 1)), GroupNode(2, 1, 1)), 3, 3)
  val uf3it2gr = new UnionFind(ChoiceNode(ChoiceNode(GroupNode(0, 1, 1), GroupNode(1, 2, 2)), SingleNode(2, 1)), 3, 2)
  val uf3it1gr = new UnionFind(ChoiceNode(ChoiceNode(SingleNode(0, 2), GroupNode(1, 3, 3)), SingleNode(2, 1)), 3, 1)
  val uf5 = new UnionFind(5)

  test("getDepth") {
    withClue("for size=3") {UnionFind.getDepth(3) should equal (2)}
    withClue("for size=4") {UnionFind.getDepth(4) should equal (2)}
    withClue("for size=5") {UnionFind.getDepth(5) should equal (3)}
    withClue("for size=8") {UnionFind.getDepth(8) should equal (3)}
  }

  test("UnionFind(Int)") {
    val uf3 = new UnionFind(3);
    assert(uf3.nodesTree === uf3it3gr.nodesTree, "new UnionFind(3)")
    uf3.groupsCount should equal (3)

    val uf5 = new UnionFind(5);
    uf5.groupsCount should equal (5)
    assert(uf5.nodesTree === ChoiceNode(
        ChoiceNode(
          ChoiceNode(GroupNode(0, 1, 1), GroupNode(1, 1, 1)),
          ChoiceNode(GroupNode(2, 1, 1), GroupNode(3, 1, 1))
        ),
        GroupNode(4, 1, 1)),
        "new UnionFind(5)"
    )
  }

  test("getByPath") {
    assert(getByPath(uf3it1gr.nodesTree, List(false, false)) === SingleNode(0, 2))
    assert(getByPath(uf3it1gr.nodesTree, List(false, true)) === GroupNode(1, 3, 3))
    assert(getByPath(uf3it1gr.nodesTree, List(true, false)) === SingleNode(2, 1))
  }

  test("getPath") {
    assert(uf3it1gr.getPath(0) === List(false, false))
    assert(uf3it1gr.getPath(1) === List(false, true))
    assert(uf3it1gr.getPath(2) === List(true, false))
  }

  test("getPath4Items") {
    val uf4 = new UnionFind(4)
    assert(uf4.getPath(0) === List(false, false))
    assert(uf4.getPath(1) === List(false, true))
    assert(uf4.getPath(2) === List(true, false))
    assert(uf4.getPath(3) === List(true, true))
  }

  test("find non-joined") {
    assert(uf3it3gr.find(0) === GroupNode(0,1,1))
    assert(uf3it3gr.find(1) === GroupNode(1,1,1))
    assert(uf3it3gr.find(2) === GroupNode(2,1,1))
  }

  test("find 1-level joined") {
    assert(uf3it2gr.find(0) === GroupNode(0,1,1))
    assert(uf3it2gr.find(1) === GroupNode(1,2,2))
    assert(uf3it2gr.find(2) === GroupNode(1,2,2))
  }

  test("find 2-level joined") {
    assert(uf3it1gr.find(0) === GroupNode(1,3,3))
    assert(uf3it1gr.find(1) === GroupNode(1,3,3))
    assert(uf3it1gr.find(2) === GroupNode(1,3,3))
  }

  test("replaceByPath") {
    val replTree = uf3it3gr.nodesTree.replaceByPath(List(false, true), GroupNode(100, 1, 1))
    assert(getByPath(replTree, List(false, true)) === GroupNode(100, 1, 1))
    assert(getByPath(replTree, List(false, false)) === GroupNode(0, 1, 1))
    assert(getByPath(replTree, List(true, false)) === GroupNode(2, 1, 1))
    assert(getByPath(uf3it3gr.nodesTree, List(false, true)) === GroupNode(1, 1, 1)) //chech that there were no side-effects
  }

  test("join 2 of 3 items") {
    val ufJoined = uf3it3gr.join(0, 1)._1;
    val gr = (for (i <- 0 until 3) yield ufJoined.find(i)).toIndexedSeq

    ufJoined.groupsCount should equal (2)

    gr(0) should equal (gr(1))
    gr(0) should not equal (gr(2))
    gr(1) should not equal (gr(2))
  }

  test("join 3 of 5 items") {
    val ufJoined = uf5.join(0, 1)._1.join(4, 1)._1
    val gr = (for (i <- 0 until 5) yield ufJoined.find(i)).toIndexedSeq

    ufJoined.groupsCount should equal (3)

    gr(0) should equal (gr(1))
    gr(0) should equal (gr(4))
    gr(4) should equal (gr(0))
    gr(0) should not equal (gr(2))
    gr(0) should not equal (gr(3))
    gr(1) should not equal (gr(2))
    gr(1) should not equal (gr(3))
    gr(2) should not equal (gr(3))
    gr(2) should not equal (gr(4))
    gr(3) should not equal (gr(4))
  }

  test("join 2 and 3 of 5 items") {
    val ufJoined = uf5.join(0, 1)._1.join(4, 1)._1.join(2, 3)._1
    val gr = (for (i <- 0 until 5) yield ufJoined.find(i)).toIndexedSeq

    ufJoined.groupsCount should equal (2)

    gr(0) should equal (gr(1))
    gr(0) should equal (gr(4))
    gr(4) should equal (gr(0))
    gr(0) should not equal (gr(2))
    gr(0) should not equal (gr(3))
    gr(1) should not equal (gr(2))
    gr(1) should not equal (gr(3))
    gr(2) should equal (gr(3))
    gr(2) should not equal (gr(4))
    gr(3) should not equal (gr(4))
  }

  test("join 4 of 5 items") {
    val ufJoined = uf5.join(0, 1)._1.join(4, 3)._1.join(1, 3)._1
    val gr = (for (i <- 0 until 5) yield ufJoined.find(i)).toIndexedSeq

    ufJoined.groupsCount should equal (2)

    gr(0) should equal (gr(1))
    gr(0) should equal (gr(3))
    gr(0) should equal (gr(4))
    gr(0) should not equal (gr(2))
    gr(1) should equal (gr(3))
    gr(1) should equal (gr(4))
    gr(1) should not equal (gr(2))
    gr(3) should equal (gr(4))
    gr(3) should not equal (gr(2))
  }

  test("getGroups") {
    def toSet(groups: Map[GroupNode, List[Int]]): Map[GroupNode, Set[Int]] = {
      groups.map {case (group, items) => group -> items.toSet}
    }
    toSet(uf3it3gr.getGroups) should equal (Map(GroupNode(0, 1, 1)-> Set(0), GroupNode(1, 1, 1) -> Set(1), GroupNode(2, 1, 1) -> Set(2)))
    toSet(uf3it2gr.getGroups) should equal (Map(GroupNode(0, 1, 1)-> Set(0), GroupNode(1, 2, 2) -> Set(2, 1)))
    toSet(uf3it1gr.getGroups) should equal (Map(GroupNode(1, 3, 3) -> Set(2, 1, 0)))
  }
}
