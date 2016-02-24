package core.clustering

import core.util.Statistics
import core.{DataSet, Observation}

import scala.collection.mutable.ListBuffer

//moveCenter constructor
class KMeansCluster private (centroid: Array[Double], members: ListBuffer[Int], dataSet: DataSet) {

  // normal (public) constructor
  def this(centroid: Array[Double], dataSet: DataSet) {
    this(centroid, new ListBuffer[Int], dataSet)
  }

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

    new KMeansCluster(stats.means,members,dataSet)
  }

  final def standardDeviation:Seq[Double] = stats.standardDeviation


}

object KMeansCluster{

}
