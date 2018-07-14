package com.haskworks.graph

trait Graph[A] {
  def length: Int
  def neighbours(node: A): Set[A]
  def neighboursExcluding(node: A, excluded: Set[A]): Set[A] = {
    val result = neighbours(node).filterNot(excluded)
    println(s"neighbours for $node: $result")
    result
  }
}
