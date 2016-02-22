package core.clustering

import core.util.Statistics
import core.{DataSet}

import scala.collection.mutable.ListBuffer

class PSOCluster(centroid: Array[Double], dataSet: DataSet) {

  private val members = new ListBuffer[Int]
  private lazy val stats: Statistics = generateStatistics

  private def generateStatistics: Statistics = {
    val memberValues = members.map( dataSet.data(_) ).toArray
    new Statistics(memberValues)
  }

  def += (n: Int): Unit = members.append(n)

  def getCentroid:Array[Double] = centroid

  final def size: Int = members.size

  final def moveCenter: KMeansCluster = {
    require( members.nonEmpty, s"Cannot move the center of an empty cluster")

    KMeansCluster(stats.means,dataSet)
  }

  final def standardDeviation:Seq[Double] = stats.standardDeviation


}

object PSOCluster{
  //Constructor
  def apply(centroid: Array[Double], dataSet: DataSet): KMeansCluster = new KMeansCluster(centroid,dataSet)
}
