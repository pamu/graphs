package com.haskworks.graph

object GraphTraversal {

  def dfs[A](source: A, graph: Graph[A], visited: Set[A]): List[List[A]] = {
    val newVisited = visited + source
    val neighbours = graph.neighboursExcluding(source, visited)
    if (neighbours.isEmpty) List(List(source))
    else neighbours.map(dfs(_, graph, newVisited))
      .toList
      .flatMap(_.map(source :: _))
  }


  def bfs[A](source: A, graph: Graph[A], visited: Set[A]): List[List[A]] = {

    @scala.annotation.tailrec
    def loop(queue: List[Vertex[A]], result: List[List[A]]): List[List[A]] = {
      if (queue.isEmpty) result
      else {
        queue.head match {
          case Node(value) =>
            val newResult = result.headOption.map { list =>
              (value :: list) :: result
            }.getOrElse {
              List(value) :: result
            }
            loop(queue.tail, newResult)
          case Sentinel => loop(queue.tail ++ List(Sentinel), result)
        }
      }
    }

    loop(Node(source) :: Sentinel :: Nil, Nil)
  }

}


