package com.haskworks.graph

case class MatrixGraph[A](graph: Map[A, Map[A, Boolean]]) extends Graph[A] {

  override def length: Int = graph.size

  override def neighbours(node: A): Set[A] =
    graph(node)
      .collect { case (neighbour, true) => neighbour }.toSet

  override def vertices: Set[A] = graph.keySet

  override val inDegrees: List[(A, Int)] = graph
    .flatMap { case (_, map) => map.filter(_._2).keys }
    .groupBy(identity)
    .mapValues(_.toList.length)
    .toList


  override def inDegrees(nodes: Set[A]): List[(A, Int)] =
    inDegrees.filter { case (x, _) => nodes(x) }

  override val edges: List[(A, List[A])] = graph.map { case (node, map) =>
      node -> map.collect { case (v, true) => v }.toList
  }.toList
}