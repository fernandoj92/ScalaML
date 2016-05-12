package core.clustering.kmeans

import core.DataSet
import core.clustering.model.{CentroidCluster, CentroidModel}
import core.util.Distances.DistanceFunc

import scala.annotation.tailrec
import scala.util.Random


/**
  * Implementation of the Kmeans algorithm.
  *
  * @param distance  the distance function that is going to be used inside the algorithm.
  * @param K the number of clusters.
  * @param maxIters maximum number of iterations.
  */
class KMeans (K: Int,
              maxIters: Int,
              distance: DistanceFunc) {

  require(maxIters > 0,
    s"The maximum number of iterations ($maxIters) cannot be lesser than 1")

  /**
    * Trains a series of clusters (the model).
    *
    * @param dataSet the dataSet that is going to be used to learn the clusters.
    * @return the model.
    */
  def train(dataSet: DataSet): CentroidModel ={
    require(dataSet.data.length >= K,
      s"K ($K) cannot be greater than the dataSet size (${dataSet.data.length})")

    // Initializes the model by giving value to the centroids.
    val clusters = initialize(dataSet)

    // This array will remember the assigned cluster of each instance of the dataSet.
    // Initially all the instances belong to the first cluster.
    val assignments = Array.fill(dataSet.data.length)(-1)

    // Assigns each instance to its nearest cluster.
    CentroidCluster.assignToClusters(dataSet, clusters, assignments, distance)

    // Initializes current iterations.
    val iterations = 1

    // Launch the recursion
    val finalClusters = iterate(dataSet, clusters, assignments, iterations)

    new CentroidModel(finalClusters, assignments, dataSet)

  }

  /**
    *
    * @param model
    * @param dataSet
    * @return
    */
  def train(model: CentroidModel, dataSet: DataSet): CentroidModel ={
    require(dataSet.data.length >= K,
      s"K ($K) cannot be greater than the dataSet size (${dataSet.data.length})")

    // Initial clusters
    val clusters = model.getClusters

    // Initial assignments
    val assignments = model.getAssignments

    // Initializes current iterations.
    val iterations = 1

    // Launch the recursion
    val finalClusters = iterate(dataSet, clusters, assignments, iterations)

    new CentroidModel(finalClusters, assignments, dataSet)

  }

  /**
    * Initializes the clusters values randomly.
    *
    * @param dataSet the dataSet that is going to be used to learn the clusters.
    * @return the initialized clusters.
    */
  private def initialize(dataSet: DataSet): List[CentroidCluster] = {
    // Creates a Set that will store the index of the random instance that will be chosen as the centroid of the cluster.
    val indexes = collection.mutable.Set(Random.nextInt(dataSet.data.length))

    // It keeps adding more centroids until its size is equal to K.
    while (indexes.size < K)
      indexes.add(Random.nextInt(dataSet.data.length))

    // TODO: It is necessary to transform it at the beginning or we would ignore 'repeated' instances
    // TODO: But, shouldn't centroids be different?

    // It creates a new cluster for each centroid.
    indexes.toList.map(x => new CentroidCluster(dataSet.data(x),dataSet))
  }

  /**
    * Recursive private method that updates the centroids values and reassigns the instances to the new clusters.
    *
    * @param _initialClusters the initial value of the centroids.
    * @param assignments the array containing the cluster that each instance belongs to.
    * @param iters current number of iterations passed.
    * @return the updated model.
    */
  @tailrec
  private def iterate(dataSet: DataSet, _initialClusters: List[CentroidCluster], assignments: Array[Int], iters : Int): List[CentroidCluster] = {

    // The clusters' centroids are moved.
    val updatedClusters = _initialClusters.map(_.moveCenter)
    // Instances are re-assigned
    val numberOfAssignments = CentroidCluster.assignToClusters(dataSet, updatedClusters, assignments, distance)

    // Stop condition of the algorithm
    if( iters >= maxIters || numberOfAssignments == 0)
      updatedClusters
    else
      iterate(dataSet, updatedClusters,assignments,iters + 1)
  }

}


