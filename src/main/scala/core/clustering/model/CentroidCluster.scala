package core.clustering.model

import core.DataSet
import core.util.Distances._
import core.util.Statistics

import scala.collection.mutable.ListBuffer

/**
  * This class defines a cluster created using the KMeans algorithm.
  *
  * @param centroid the cluster's centroid.
  * @param dataSet the DataSet containing all the data instances.
  */
class CentroidCluster(centroid: Array[Double], dataSet: DataSet) {

  /** The necessary statistics of the cluster. It will only be computed when called. */
  private lazy val stats: Statistics = generateStatistics

  /** The members index list of the cluster. */
  private val members : ListBuffer[Int] = new ListBuffer[Int]()

  /**
    * Returns the standard deviation of the cluster.
    *
    * @return the standard deviation of the cluster.
    */
  final def standardDeviation: Array[Double] = stats.standardDeviation

  /**
    * Returns the Sum distances of each instance to the centroid
    * @return the Sum distances of each instance to the centroid
    */
  final def distancesSum(distanceFunc: DistanceFunc): Double = {
    var sum = 0.0
    for(i <- dataSet.data.indices)
      sum = distanceFunc(dataSet.data(i), centroid)

    sum
  }


  /**
    * Returns the number of members the cluster has.
    *
    * @return the number of members the cluster has.
    */
  final def size: Int = members.size

  /**
    * Returns the cluster's members.
    *
    * @return the cluster's members.
    */
  final def getMembers: ListBuffer[Int] = members

  /**
    * Returns the centroid of the cluster.
    *
    * @return the centroid of the cluster.
    */
  final def getCentroid: Array[Double] = centroid

  /**
    * Adds a new member to the cluster by storing its index.
    *
    * @param index the member's index.
    */
  def +=(index: Int): Unit = members.append(index)

  /**
    * Returns an updated cluster with new centroid.
    *
    * @return an updated cluster with new centroid.
    */
  final def moveCenter: CentroidCluster = {
    require( members.nonEmpty, s"Cannot move the center of an empty cluster")

    // Creates the new cluster
    val newCluster = new CentroidCluster(stats.means,dataSet)

    // TODO: Instead of assgining all of them and then reassign them, why not simply assign them only once?
    // TODO: Some way to pass a copied version of the members list
    this.members.foreach(newCluster.+=(_))

    // Returns the new cluster.
    newCluster
  }

  /**
    * Returns the necessary statistics of the cluster.
    *
    * @return the necessary statistics of the cluster.
    */
  private def generateStatistics: Statistics = {
    val memberValues = members.map( dataSet.data(_) ).toArray
    new Statistics(memberValues)
  }

}

object CentroidCluster{

  /**
    * Private method that assigns each instance of the dataSet to it nearest cluster.
    *
    * @param dataSet the dataSet that is going to be used to learn the clusters.
    * @param clusters current model.
    * @param assignments the array containing the assigned cluster for each data instance.
    * @return the updated model and the number of assignments that have been made.
    */
  // TODO: (Borrar comment???) Aqui esta reasignando sin tener en cuenta si esa instancia ya pertenecia a dicho cluster
  def assignToClusters(dataSet: DataSet, clusters: List[CentroidCluster], assignments: Array[Int], distanceFunc: DistanceFunc): (List[CentroidCluster], Int) ={

    var numberOfAssignments = 0

    for(instanceIndex <- dataSet.data.indices) {
      val nearestCluster = getNearestCluster(clusters, dataSet.data(instanceIndex), distanceFunc)
      // re-assign if the observations does not belong to its nearest cluster
      if (nearestCluster != assignments(instanceIndex)){
        // Re-assign ONLY if the instance does not belong to its nearest cluster
        clusters(nearestCluster) += instanceIndex
        assignments(instanceIndex) = nearestCluster
        numberOfAssignments = numberOfAssignments + 1
      }
    }
    // Returns the new model and the number of assignments that have been made.
    (clusters,numberOfAssignments)
  }

  /**
    * Returns the nearest cluster for a data instance
    *
    * @param clusters current model.
    * @param instance the data instance to be assigned.
    * @return the nearest cluster's index.
    */
  def getNearestCluster(clusters: List[CentroidCluster], instance: Array[Double], distance: DistanceFunc): Int ={

    // All the distances to the clusters are calculated.
    val distances = clusters.zipWithIndex.map{
      case (cluster,index) => (distance(instance,cluster.getCentroid),index)
    }
    // Minimum distance (nearest cluster).
    val min = distances.minBy(_._1)
    // Nearest cluster.
    val nearestCluster = min._2
    // Returns the nearest cluster.
    nearestCluster
  }

}
