package com.haskworks.graph

object DetectCycleUnDirected {

  def detectCycle[A](source: A, graph: Graph[A]): Boolean = {

    def loop(parent: Option[A], currentNode: A, visited: Set[A]): Boolean = {
      val newVisited = visited + currentNode
      // Get all neighbours excluding
      graph.neighboursExcluding(currentNode, parent.toSet).map { node =>
        if (newVisited(node)) true
        else loop(Some(currentNode), node, newVisited)
      }.foldLeft(false)(_ || _)
    }

    loop(None, source, Set())
  }

}
