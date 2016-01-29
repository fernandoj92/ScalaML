package core.util

/**
 * Created by Fernando on 28/01/2016.
 */
object Types {

  type NormalizedDouble = Double

  implicit def intToDouble(n: Double): NormalizedDouble = n.toDouble
}
