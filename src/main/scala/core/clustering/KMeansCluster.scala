package core.clustering

import core.DataSet
import core.util.Statistics

import scala.collection.mutable.ListBuffer

/**
  * This class defines a cluster created using the Kmeans algorithm.
  * @param centroid the cluster's centroid.
  * @param dataSet the DataSet containing all the data instances.
  */
class KMeansCluster (centroid: Array[Double], dataSet: DataSet) {

  /** The necessary statistics of the cluster. It will only be computed when called. */
  private lazy val stats: Statistics = generateStatistics

  /** The members index list of the cluster. */
  private val members: ListBuffer[Int] = new ListBuffer[Int]()

  /**
    * Returns the standard deviation of the cluster.
    * @return the standard deviation of the cluster.
    */
  final def standardDeviation: Array[Double] = stats.standardDeviation

  /**
    * Returns the number of member the cluster has.
    * @return the number of member the cluster has.
    */
  final def size: Int = members.size

  /**
    * Returns the centroid of the cluster.
    * @return the centroid of the cluster.
    */
  final def getCentroid: Array[Double] = centroid

  /**
    * Adds a new member to the cluster by storing its index.
    * @param index the member's index.
    */
  def +=(index: Int): Unit = members.append(index)

  /**
    * Returns an updated cluster with new centroid.
    * @return an updated cluster with new centroid.
    */
  final def moveCenter: KMeansCluster = {
    require( members.nonEmpty, s"Cannot move the center of an empty cluster")

    // Creates the new cluster
    val newCluster = new KMeansCluster(stats.means,dataSet)

    // TODO: Instead of assgining all of them and then reassign them, why not simply assign them only once?
    this.members.foreach(newCluster.+=(_))

    // Returns the new cluster.
    newCluster
  }

  /**
    * Returns the necessary statistics of the cluster.
    * @return the necessary statistics of the cluster.
    */
  private def generateStatistics: Statistics = {
    val memberValues = members.map( dataSet.data(_) ).toArray
    new Statistics(memberValues)
  }

}

object KMeansCluster{

}
