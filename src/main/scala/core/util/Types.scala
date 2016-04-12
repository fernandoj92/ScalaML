package core.util

/**
 * Created by Fernando on 28/01/2016.
 */
object Types {

  /** This type simply represent a double value that has been normalized. */
  type NormalizedDouble = Double

  /** An implicit converstion from Int to Double. */
  implicit def intToDouble(n: Int): Double = n.toDouble
}
