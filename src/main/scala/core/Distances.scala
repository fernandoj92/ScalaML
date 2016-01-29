package core



object Distances {

  @throws(classOf[IllegalArgumentException])
  def Euclidean[A <: Double, B <: Double](x: Array[A], y: Array[B]): Double = {
    require( x.length == y.length,
      s"Distance.euclidean Vectors have different size ${x.length} and ${y.length}")

    Math.sqrt((x, y).zipped.map{case (u,v) => u-v}.map( w => w*w).sum)
  }

  @throws(classOf[IllegalArgumentException])
  def Manhattan[A <: Double, B <: Double](x: Array[A], y: Array[B]): Double = {
    require( x.length == y.length,
      s"Distance.manhattan Vectors have different size ${x.length} and ${y.length}")

    (x,y).zipped.map{ case (u,v) => Math.abs(u-v)}.sum
  }
}
