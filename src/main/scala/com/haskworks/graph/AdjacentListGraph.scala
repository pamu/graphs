package com.haskworks.graph

case class AdjacentListGraph[A](length: Int, graph: Map[A, List[A]]) extends Graph[A] {
  override def neighbours(node: A): Set[A] = graph(node).toSet
}