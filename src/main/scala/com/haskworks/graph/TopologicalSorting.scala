package com.haskworks.graph

object TopologicalSorting {


  def topologicalSort[A](graph: Graph[A]): List[A] = {

    def loop(rest: List[(A, List[A])]): List[A] = rest match {
      case Nil => List.empty[A]
      case ::(head, tl) =>
        if (head._2.isEmpty) head._1 :: loop(remove(head._1, tl))
        else loop(tl :+ head)
    }

    def remove(node: A, from: List[(A, List[A])]): List[(A, List[A])] =
      from.diff(List(node)).map { case (x, xs) =>
        x -> (xs diff List(node))
      }

    loop(graph.edges)
  }

}
