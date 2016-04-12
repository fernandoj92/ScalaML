package core.util

/**
  * This class calculates some standard statistics that can be used in other continuous-value algorithms.
  * It makes use of dynamic programming to be more efficient.
  * @param values the matrix of values representing a list of continuous-valued rows.
  */
// Arrays are more efficient that other data structures. However, it is not possible to use Seq because Arrays are
// invariant, and therefore Array[Array] doesn't work.
class Statistics(values: Array[Array[Double]]) {

  /** It calculates the column summation of the matrix. */
  // The transpose makes it a bit more inefficient
  val sums: Array[Double] = values.transpose.map(_.sum)

  /** It calculates the column means of the matrix. */
  val means: Array[Double] = sums.map(_ / values.size)

  /** It calculates the matrix variances in a lazy way (only calculated when requested). */
  lazy val variances: Array[Double] = {
    // Iterates through the matrix row indices.
    (for(i <- values.indices)
      yield (for(j <- values(i).indices)
        yield Math.pow(values(i)(j) - means(j), 2) / values.size).sum).toArray
  }

  /** Calculates the standard deviation of the matrix of values in a lazy way (only calculated when requested). */
  lazy val standardDeviation: Array[Double] = variances.map(Math.sqrt(_))

}
