package core.clustering.kmeans

import core.DataSet
import core.clustering.model.{CentroidCluster, CentroidModel}
import core.util.Distances.DistanceFunc

import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}


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
  def train(dataSet: DataSet): Option[CentroidModel] = Try{
    require(dataSet.data.length >= K,
      s"K ($K) cannot be greater than the dataSet size (${dataSet.data.length})")

    // Initializes the model by giving value to the centroids.
    val initialClusters = initialize(dataSet)

    // This array will remember the assigned cluster of each instance of the dataSet.
    // Initially all the instances belong to the first cluster.
    val assignments = Array.fill(dataSet.data.length)(-1)

    // Assigns each instance to its nearest cluster.
    val assignedClusters = CentroidCluster.assignToClusters(dataSet, initialClusters, assignments, distance)._1

    // Initializes current iterations.
    val iterations = 0

    // Launch the recursion
    iterate(dataSet, new CentroidModel(assignedClusters, dataSet), assignments, iterations)

  }match {
    case Success(model) => Some(model)
    case Failure(exception) =>
      // TODO: Maybe log the exception?
      None
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
    * @param _initialModel the initial value of the centroids.
    * @param assignments the array containing the cluster that each instance belongs to.
    * @param iters current number of iterations passed.
    * @return the updated model.
    */
  @tailrec
  private def iterate(dataSet: DataSet, _initialModel: CentroidModel, assignments: Array[Int], iters : Int): CentroidModel = {

    // The clusters' centroids are moved.
    val updatedClusters = _initialModel.getClusters.map(_.moveCenter)
    // Instances are re-assigned
    val result = CentroidCluster.assignToClusters(dataSet, updatedClusters, assignments, distance)
    // The updated model.
    val newClusters = result._1
    //The number of assignments
    val numberOfAssignments = result._2

    // Stop condition of the algorithm
    if( iters >= maxIters || numberOfAssignments == 0)
      new CentroidModel(newClusters, dataSet)
    else
      iterate(dataSet, new CentroidModel(newClusters, dataSet),assignments,iters + 1)
  }

}

object KMeans{


}

