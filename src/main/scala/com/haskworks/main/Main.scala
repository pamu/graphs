package com.haskworks.main

import com.haskworks.graph.{AdjacentListGraph, DetectCycleUnDirected, GraphTraversal, Path}

object Main {

  def main(args: Array[String]): Unit = {
    val graph = AdjacentListGraph(
      6,
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
      4,
      Map(
        1 -> List(2),
        2 -> List(1, 3),
        3 -> List(2, 4),
        4 -> List(3)
      )
    )

    val simpleGraphCycle = DetectCycleUnDirected.detectCycle(1, simpleGraph)

    println(s"cycle: $simpleGraphCycle")
  }

}
