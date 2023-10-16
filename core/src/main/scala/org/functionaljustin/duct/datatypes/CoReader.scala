package org.functionaljustin.duct.datatypes

case class CoReader[R,A](extract: A, ask: R):
  def map[B](f: A => B): CoReader[R,B] = CoReader(f(extract),ask)
  def duplicate: CoReader[R, CoReader[R,A]] = CoReader(this, ask)
