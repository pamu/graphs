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


  def bfs[A](source: A, graph: Graph[A]): List[List[A]] = {

    @scala.annotation.tailrec
    def loop(queue: List[Vertex[A]], visited: Set[A], result: List[List[A]]): List[List[A]] = {

      println(s"queue: $queue visited: $visited result: $result")

      if (queue.isEmpty) result
      else {
        queue.head match {
          case Node(value) =>

            val neighbours = graph.neighboursExcluding(value, visited)

            val newResult = result
              .headOption
              .map(list => (value :: list) :: result.tail)
              .getOrElse(List(value) :: result)

            val newVisited = visited + value ++ neighbours

            val newQueue = queue.tail ++ neighbours.map(Node(_))

            loop(newQueue, newVisited, newResult)
          case Sentinel =>
            // If queue is empty do not add sentinel
            if (queue.tail.isEmpty) result
            else loop(queue.tail ++ List(Sentinel), visited, List.empty[A] :: result)
        }
      }
    }

    loop(Node(source) :: Sentinel :: Nil, Set(), Nil)
  }

}


