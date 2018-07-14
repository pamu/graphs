package com.haskworks.graph

object Path {

  // return first path found
  def find[A](start: A, destination: A, graph: Graph[A]): Option[List[A]] = {

    def loop(currentNode: A, path: List[A], visited: Set[A]): (List[A], Boolean) = {
      if (currentNode == destination) (currentNode :: path, true)
      else if (graph.neighboursExcluding(currentNode, visited).isEmpty) (currentNode :: path, false)
      else {
        val neighbours = graph.neighboursExcluding(currentNode, visited)
        val newVisited = visited + currentNode
        neighbours.map { node =>
          loop(node, currentNode :: path, newVisited)
        }.collectFirst {
          case r if r._2 => r
        }.getOrElse((path, false))
      }
    }

    val result = loop(start, Nil, Set())
    if (result._2) Some(result._1) else None
  }

  def findAll[A](start: A, destination: A, graph: Graph[A]): List[List[A]] = {

    def loop(currentNode: A, path: List[A], visited: Set[A]): List[(List[A], Boolean)] = {
      if (currentNode == destination) List((currentNode :: path, true))
      else if (graph.neighboursExcluding(currentNode, visited).isEmpty) List((currentNode :: path, false))
      else {
        val neighbours = graph.neighboursExcluding(currentNode, visited)
        val newVisited = visited + currentNode

        val results = neighbours.toList.flatMap { node =>
          loop(node, currentNode :: path, newVisited)
        }.collect {
          case r if r._2 => r
        }

        if (results.isEmpty) List((path, false)) else results
      }
    }

    loop(start, Nil, Set()).collect { case r if r._2 => r._1 }
  }

}
