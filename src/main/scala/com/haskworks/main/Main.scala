package com.haskworks.main

import com.haskworks.graph._

object Main {

  def main(args: Array[String]): Unit = {
    val graph = AdjacentListGraph(
      Map(
        1 -> List(2, 3),
        2 -> List(1, 4, 5),
        3 -> List(1, 5),
        4 -> List(2, 5, 6),
        5 -> List(2, 3, 4, 6),
        6 -> List(4, 5)
      )
    )

    val dfsResult = GraphTraversal
      .dfs(1, graph, Set())
      .reverse
      .map(_.mkString(", "))
      .mkString("\n")

    println(s"dfs:\n$dfsResult")

    val bfsResult = GraphTraversal
      .bfs(1, graph)
      .reverse
      .map(_.mkString(", "))
      .mkString("\n")

    println(s"bfs:\n$bfsResult")

    val path = Path.find(1, 5, graph)

    println(s"found path: $path")

    val paths = Path.findAll(1, 5, graph)

    println(s"found paths: $paths")

    val cycle = DetectCycleUnDirected.detectCycle(1, graph)

    println(s"cycle: $cycle")

    val simpleGraph = AdjacentListGraph(
      Map(
        1 -> List(2),
        2 -> List(1, 3),
        3 -> List(2, 4),
        4 -> List(3)
      )
    )

    val simpleGraphCycle = DetectCycleUnDirected.detectCycle(1, simpleGraph)

    println(s"cycle: $simpleGraphCycle")

    val directedCycleGraph = AdjacentListGraph(
      Map(
        1 -> List(2, 3),
        2 -> List(3),
        3 -> List(4),
        4 -> List(2)
      )
    )

    val directedCycle = DetectCycleDirected.detectCycle(1, directedCycleGraph)

    println(s"directed cycle: $directedCycle")

    val DAG = AdjacentListGraph(
      Map(
        0 -> List(),
        1 -> List(),
        2 -> List(3),
        3 -> List(1),
        4 -> List(0, 1),
        5 -> List(0, 2)
      )
    )

    val result = TopologicalSorting.topologicalSort(DAG)

    println(s"topological sort: $result")

    val acyclic = AdjacentListGraph(
      Map(
        (1, List(3, 5)),
        (2, List(3)),
        (3, List(4, 5)),
        (4, List()),
        (5, List())
      )
    )

    val finalResult = TopologicalSorting.topologicalSort(acyclic)

    println(s"final result: $finalResult")
  }

}
