package com.haskworks.graph

object DetectCycleDirected {

  def detectCycle[A](source: A, graph: Graph[A]): Boolean = {

    def loop(current: A, trail: List[A], visited: Set[A]): Boolean = {
      val newVisited = visited + current
      val newTrail = current :: trail
      graph.neighboursExcluding(current, Set()).map { node =>
        if (newTrail contains node) true
        else if (!newVisited(node))loop(node, newTrail, newVisited)
        else false
      }.foldLeft(false)(_ || _)
    }

    loop(source, Nil, Set())
  }

}
