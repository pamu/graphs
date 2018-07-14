package com.haskworks.main

import com.haskworks.graph.{AdjacentListGraph, GraphTraversal}

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
  }

}
