package com.haskworks.graph

case class AdjacentListGraph[A](graph: Map[A, List[A]]) extends Graph[A] {

  override def length: Int = graph.size

  override def neighbours(node: A): Set[A] = graph(node).toSet

  override def vertices: Set[A] = graph.keySet

  override val inDegrees: List[(A, Int)] = {
    val result = graph.flatMap(_._2)
      .groupBy(identity)
      .mapValues(_.toList.length)
      .toList
    val zeroInDegree = vertices -- result.map(_._1).toSet
    result ++ zeroInDegree.map(n => (n, 0)).toList
  }


  override def inDegrees(nodes: Set[A]): List[(A, Int)] =
    inDegrees.filter { case (x, _) => nodes(x) }

  override val edges: List[(A, List[A])] = graph.toList
}