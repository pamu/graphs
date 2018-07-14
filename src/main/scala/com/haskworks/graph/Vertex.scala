package com.haskworks.graph

sealed trait Vertex[+A]

case class Node[A](value: A) extends Vertex[A]
case object Sentinel extends Vertex[Nothing]

case class State[A](queue: List[Vertex[A]], result: List[List[A]], visited: Set[A])