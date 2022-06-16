package traffic

import cats.data.State
import cats.implicits._

object FunctionalDijkstra {

  type Node = (String, String)

  case class DijkstraState(
      unvisitedNodes: Set[Node],
      temptativeDistances: Map[Node, (List[Node], Double)]
  )

  type DijkstraStateM[A] = State[DijkstraState, A]

  def setUnvisitedNodes(nodes: Set[Node]): DijkstraStateM[Unit] =
    State.modify(s => s.copy(unvisitedNodes = nodes))

  def getUnvisitedNodes(): DijkstraStateM[Set[Node]] =
    State.inspect(_.unvisitedNodes)

  def assignInitialDistance(initialNode: Node): DijkstraStateM[Unit] =
    State.modify(s => {
      s.copy(temptativeDistances =
        Map(
          (s.unvisitedNodes
            .map(x =>
              if (initialNode == x) x -> (Nil, 0L)
              else x -> (Nil, Double.PositiveInfinity)
            ): Set[(Node, (List[Node], Double))]).toList: _*
        )
      )
    })

  def getUnvisitedNeighbors(
      currentNode: Node,
      neighbourMap: Map[(Node, Node), Double]
  ): DijkstraStateM[Set[Node]] = State { s =>
    val currentNodeNeighbors =
      neighbourMap.filter(_._1._1 == currentNode).keySet.map(_._2)
    val unvisitedNeihbors =
      currentNodeNeighbors.filter(s.unvisitedNodes.contains(_))
    (s, unvisitedNeihbors)
  }

  def computeTentativeDistanceForNode(
      currentNode: Node,
      neighborNode: Node,
      neighbourMap: Map[(Node, Node), Double]
  ): DijkstraStateM[Double] =
    State.apply { s =>
      val currentDistance =
        s.temptativeDistances.get(currentNode)
      val distanceFromCurrentNode =
        neighbourMap.get((currentNode, neighborNode))
      val res =
        (currentDistance, distanceFromCurrentNode).mapN((a, b) => a._2 + b)
      (s, res.getOrElse(Double.PositiveInfinity))
    }

  def computeTentativeDistancesForNeighbors(
      currentNode: Node,
      neighbourMap: Map[(Node, Node), Double]
  ): DijkstraStateM[Map[Node, Double]] =
    for {
      unvisitedNeighbors <- getUnvisitedNeighbors(currentNode, neighbourMap)
      unvisitedNeighborList = unvisitedNeighbors.toList
      listOfDistances <- unvisitedNeighborList.traverse(neighborNode =>
        computeTentativeDistanceForNode(
          currentNode,
          neighborNode,
          neighbourMap
        )
      )
    } yield Map(unvisitedNeighborList.zip(listOfDistances): _*)

  def updateTentativeDistances(
      currentNode: Node,
      newTentativeDistances: Map[Node, Double]
  ): DijkstraStateM[Unit] = State.modify { s =>
    val newDistances = s.temptativeDistances.map { entry =>
      val (node, (currentRoute, currentDistance)) = entry
      val newTentativeDistance =
        newTentativeDistances.get(node).getOrElse(Double.PositiveInfinity)
      if (newTentativeDistance < currentDistance) {
        val (newRoute, _) = s.temptativeDistances
          .get(currentNode)
          .getOrElse((Nil, Double.PositiveInfinity))
        node -> (newRoute.appended(currentNode), newTentativeDistance)
      } else {
        entry
      }
    }
    s.copy(
      temptativeDistances = newDistances
    )
  }

  def computeShortestDistanceForNeighbors(
      currentNode: Node,
      neighbourMap: Map[(Node, Node), Double]
  ): DijkstraStateM[(Node, Double)] =
    for {
      newTentativeDistances <- computeTentativeDistancesForNeighbors(
        currentNode,
        neighbourMap
      )
      _ <- updateTentativeDistances(currentNode, newTentativeDistances)
      unvisitedNodes <- getUnvisitedNodes()
      _ <- setUnvisitedNodes(unvisitedNodes - currentNode)
      nextNodeAndMinDistanceToNextNode <-
        getSmallestTentativeDistanceForUnvisitedNodes()
      (nextNode, minimalDistanceToNextNode) = nextNodeAndMinDistanceToNextNode
    } yield (nextNode, minimalDistanceToNextNode)

  def getSmallestTentativeDistanceForUnvisitedNodes()
      : DijkstraStateM[(Node, Double)] =
    State.inspect { s =>
      s.unvisitedNodes.foldLeft(("", "") -> Double.PositiveInfinity)(
        (prev, current) => {
          val (_, prevMin) =
            s.temptativeDistances
              .get(prev._1)
              .getOrElse((Nil, Double.PositiveInfinity))
          val (_, currentVal) =
            s.temptativeDistances
              .get(current)
              .getOrElse((Nil, Double.PositiveInfinity))
          if (currentVal < prevMin) {
            current -> currentVal
          } else {
            prev
          }
        }
      )
    }

  def computeShortestDistanceAux(
      currentNode: Node,
      finalNode: Node,
      neighbourMap: Map[(Node, Node), Double]
  ): DijkstraStateM[(List[Node], Double)] = (for {
    res <- computeShortestDistanceForNeighbors(currentNode, neighbourMap)
    unvisitedNodes <- getUnvisitedNodes()
    (newCurrentNode, minimalDistanceNode) = res
  } yield {
    if (
      // this are the standard conditions of Dijkstra
      !unvisitedNodes.contains(finalNode) || minimalDistanceNode.isInfinite()
    ) {
      State.inspect((s: DijkstraState) =>
        s.temptativeDistances
          .get(finalNode)
          .getOrElse((Nil, Double.PositiveInfinity))
      )
    } else {
      computeShortestDistanceAux(
        newCurrentNode,
        finalNode,
        neighbourMap
      )
    }
  }).flatten

  def computeShortestDistance(
      initialNode: Node,
      finalNode: Node,
      allNodes: Set[Node],
      neighbourMap: Map[(Node, Node), Double]
  ): (List[Node], Double) =
    (for {
      _ <- setUnvisitedNodes(allNodes)
      _ <- assignInitialDistance(initialNode)
      shortestPathAndDistance <- computeShortestDistanceAux(
        initialNode,
        finalNode,
        neighbourMap
      )
    } yield shortestPathAndDistance)
      .run(FunctionalDijkstra.DijkstraState(Set(), Map()))
      .value
      ._2

}
