package scheduler

import collection.mutable.WrappedArray
import language.implicitConversions

object OptArray {
  def apply(size: Int) = new OptArray(size)
  implicit def OptToArray(o: OptArray) = new WrappedArray.ofInt(o.opt)
}

/**
 * A wrapper class for Array[Int] that defines apply(-1) to be equal to 0.
 * 
 * @param size  the size of Array to create
 */
class OptArray(size: Int) {
  private val opt = new Array[Int](size)
  
  def apply(i: Int) = if (i == -1) 0 else opt(i)
  def update(i: Int, x: Int) = opt(i) = x
}