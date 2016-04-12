package core.util


/**
  * This object takes the responsibility of calculating the distances that are going to be used in the project's algorithms.
  */
object Distances {

  /** This type defines the structure of all the allowed distance functions. */
  // TODO: This type of distanceFunc will require to define implicit conversions from every Double related type, to double.
  type DistanceFunc = (Array[Double], Array[Double]) => Double

  /**
    * This method calculates the euclidean distance between two vectors whose type can be converted to double.
    * @param x the first vector.
    * @param y the second vector.
    * @tparam A first vector's type, which will be implicitly converted to double if possible, if not it will send an error.
    * @tparam B second vector's type, which will be implicitly converted to double if possible, if not it will send an error.
    * @throws java.lang.IllegalArgumentException when both vectors have different size.
    * @return the euclidean distance.
    */
  @throws(classOf[IllegalArgumentException])
  def Euclidean[A, B](x: Array[A], y: Array[B])
                     (implicit f: A => Double, g: B => Double): Double = {
    require( x.length == y.length,
      s"Distance.euclidean Vectors have different size ${x.length} and ${y.length}")

    Math.sqrt((x, y).zipped.map{case (u,v) => u-v}.map( w => w*w).sum)
  }

  /**
    * This method calculates the manhattan distance between two vectors whose type can be converted to double.
    * @param x the first vector.
    * @param y the second vector.
    * @tparam A first vector's type, which will be implicitly converted to double if possible, if not it will send an error.
    * @tparam B second vector's type, which will be implicitly converted to double if possible, if not it will send an error.
    * @throws java.lang.IllegalArgumentException when both vectors have different size.
    * @return the manhattan distance.
    */
  // This method uses view bounds instead of implicit parameters, just to point that exists this possibility
  // for representing the same as above.
  @throws(classOf[IllegalArgumentException])
  def Manhattan[A <% Double, B <% Double](x: Array[A], y: Array[B]): Double = {
    require( x.length == y.length,
      s"Distance.manhattan Vectors have different size ${x.length} and ${y.length}")

    (x,y).zipped.map{ case (u,v) => Math.abs(u-v)}.sum
  }
}
