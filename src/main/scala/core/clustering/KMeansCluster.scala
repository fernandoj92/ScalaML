package core.clustering

import core.{DataSet, Observation}

import scala.collection.mutable.ListBuffer

class KMeansCluster(centroid: Array[Double], dataSet: DataSet) {

  val members = new ListBuffer[Int]

  def += (n: Int): Unit = members.append(n)

  def moveCenter: KMeansCluster = {
    require( members.nonEmpty, s"Cannot move the center of an empty cluster")

    // Hacemos la media de cada dimension y ese pasa a ser el nuevo centroide
    // SUSTITUIR POR LA CREACIÓN DEL OBJETO DE STATISTICS, quizas en su constructor tiene mas sentido
    // con el dataset creamos el objeto de estadisticas asociado
    val memberValues = members.map( dataSet.data(_) ).toArray
    val sums = memberValues.transpose.map(_.sum)
    val means = sums.map( _ / members.size)
    KMeansCluster(means,dataSet)
  }

  def standardDeviation

  // Tiene sentido definir una clase de Statistics con valores lazy, ya que solo se utilizan si se piden
  // y como son en cascada si pedimos algo de mas abajo pues ya genera lo anterior y si solo necesitamos
  // lo basico pues solo almacena lo basico
}

object KMeansCluster{
  //Constructor
  def apply(centroid: Array[Double], dataSet: DataSet): KMeansCluster = new KMeansCluster(centroid,dataSet)
}
