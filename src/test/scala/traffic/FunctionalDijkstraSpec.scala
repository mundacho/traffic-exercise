package traffic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FunctionalDijkstraSpec extends AnyFlatSpec with Matchers {

  val a1 = ("A", "1")

  val a2 = ("A", "2")

  val a3 = ("A", "3")

  val b1 = ("B", "1")

  val b2 = ("B", "2")

  val b3 = ("B", "3")

  val c1 = ("C", "1")

  val c2 = ("C", "2")

  val c3 = ("C", "3")

  val allNodes = Set(
    a1,
    a2,
    a3,
    b1,
    b2,
    b3,
    c1,
    c2,
    c3
  )

  val neighbourMap = Map(
    (a1 -> b1) -> 1.0,
    (a2 -> b2) -> 1.0,
    (a1 -> a2) -> 1.0,
    (b1 -> b2) -> 1.0,
    (b2 -> a2) -> 1.0,
    (a1 -> b2) -> 3.0,
    (b1 -> a2) -> 1.0,
    (b1 -> c1) -> 1.0,
    (b2 -> c2) -> 1.0,
    (a2 -> a3) -> 1.0,
    (b2 -> b3) -> 1.0,
    (a3 -> b3) -> 1.0,
    (a1 -> c1) -> 0.5
  )

  "FunctionalDijkstraSpec" should "find the direct through street" in {
    val (segment, time) = FunctionalDijkstra
      .computeShortestDistance(
        a1,
        b1,
        allNodes,
        neighbourMap
      )
    time shouldEqual 1.0
    segment shouldEqual a1 :: Nil
  }
  it should "find the direct path through avenue" in {
    val (segment, time) = FunctionalDijkstra
      .computeShortestDistance(
        a1,
        a2,
        allNodes,
        neighbourMap
      )
    time shouldEqual 1.0
    segment shouldEqual a1 :: Nil
  }
  it should "find the shortest direct path" in {
    val (segment, time) = FunctionalDijkstra
      .computeShortestDistance(
        b1,
        a2,
        allNodes,
        neighbourMap
      )
    time shouldEqual 1.0
    segment shouldEqual b1 :: Nil
  }
  it should "find the direct path shorter (shortcut)" in {
    val (segment, time) = FunctionalDijkstra
      .computeShortestDistance(
        a1,
        c1,
        allNodes,
        neighbourMap
      )
    time shouldEqual 0.5
    segment shouldEqual a1 :: Nil
  }
  it should "find the longer but faster" in {
    val (segment, time) = FunctionalDijkstra
      .computeShortestDistance(
        a1,
        b2,
        allNodes,
        neighbourMap
      )
    time shouldEqual 2.0
    println(segment)
    segment shouldEqual a1 :: a2 :: Nil
  }
  it should "find the longer but faster (three segments)" in {
    val (segment, time) = FunctionalDijkstra
      .computeShortestDistance(
        a1,
        b3,
        allNodes,
        neighbourMap
      )
    time shouldEqual 3.0
    println(segment)
    segment shouldEqual a1 :: a2 :: b2 :: Nil
  }
  it should "detect unatainable" in {
    val (segment, time) = FunctionalDijkstra
      .computeShortestDistance(
        a1,
        c3,
        allNodes,
        neighbourMap
      )
    time.isInfinite shouldEqual true
    segment shouldEqual List()
  }

}
