package core.util

// If we want to work with Arrays efficiently we cannot use Seq,
// because Arrays are invariant and so Array(Array) doesnt work
class Statistics(values: Array[Array[Double]]) {

  // Ineficiente(transpose)
  // Es necesario el lazy? (No)
  val sums:Array[Double] = values.transpose.map(_.sum)

  val means:Array[Double] = sums.map(_ / values.size)

  //Ineficiente???
  lazy val variances :Array[Double] = {
    (for(i <- values.indices)
      yield (for(j <- values(i).indices)
        yield Math.pow(values(i)(j) - means(j), 2) / values.size).sum).toArray
  }

  lazy val standardDeviation:Array[Double] = variances.map(Math.sqrt(_))

}
