package com.haskworks.graph

case class MatrixGraph[A](length: Int, graph: Map[A, Map[A, Boolean]]) extends Graph[A] {
  override def neighbours(node: A): Set[A] =
    graph(node)
      .collect { case (neighbour, true) => neighbour }.toSet
}