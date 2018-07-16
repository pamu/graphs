package com.haskworks.graph

trait Graph[A] {
  def length: Int
  def vertices: Set[A]
  def edges: List[(A, List[A])]
  def neighbours(node: A): Set[A]
  def neighboursExcluding(node: A, excluded: Set[A]): Set[A] =
    neighbours(node).filterNot(excluded)
  val inDegrees: List[(A, Int)]
  def inDegrees(nodes: Set[A]): List[(A, Int)]
}
