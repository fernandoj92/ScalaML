package core.clustering

import core.DataSet
import core.clustering.KMeans.KMeansModel
import core.util.Distances.DistanceFunc

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try, Random}


/**
  * Implementation of the Kmeans algorithm.
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
    * @param dataSet the dataSet that is going to be used to learn the clusters.
    * @return the model.
    */
  def train(dataSet: DataSet): Option[KMeansModel] = Try{
    require(dataSet.data.length >= K,
      s"K ($K) cannot be greater than the dataSet size (${dataSet.data.length})")

    // Initializes the model by giving value to the centroids.
    val initialModel = initialize(dataSet)

    // This array will remember the assigned cluster of each instance of the dataSet.
    // Initially all the instances belong to the first cluster.
    val assignments = Array.fill(dataSet.data.length)(0)

    // Assigns each instance to a cluster.
    val assignedModel = assignToClusters(dataSet, initialModel, assignments)._1

    // Initializes current iterations.
    val iterations = 0

    // Launch the recursion
    iterate(dataSet, assignedModel, assignments, iterations)

  }match {
    case Success(clusters) => Some(clusters)
    case Failure(exception) => None
  }

  /**
    * Initializes the clusters values randomly.
    * @param dataSet the dataSet that is going to be used to learn the clusters.
    * @return the initialized clusters.
    */
  private def initialize(dataSet: DataSet): KMeansModel = {
    // Creates a Set that will store the index of the random instance that will be chosen as the centroid of the cluster.
    val indexes = collection.mutable.Set(Random.nextInt(dataSet.data.length))

    // It keeps adding more centroids until its size is equal to K.
    while (indexes.size < K)
      indexes.add(Random.nextInt(dataSet.data.length))

    // TODO: It is necessary to transform it at the beginning or we would ignore 'repeated' instances
    // TODO: But, shouldn't centroids be different?

    // It creates a new cluster for each centroid.
    indexes.toList.map(x => new KMeansCluster(dataSet.data(x),dataSet))
  }

  /**
    * Recursive private method that updates the centroids values and reassigns the instances to the new clusters.
    * @param _initialModel the initial value of the centroids.
    * @param assignments the array containing the cluster that each instance belongs to.
    * @param iters current number of iterations passed.
    * @return the updated model.
    */
  // Es necesario modificar los metodos para poder tener en cuenta si no se han producido reasignaciones,
  // ya que es una condición de parada
  // Tambien es necesario mover el centroide
  // AL SER RECURSIVO NO ES NECESARIO GUARDAR LOS CLUSTERS ya que los iremos pasando
  @tailrec
  private def iterate(dataSet: DataSet, _initialModel: KMeansModel, assignments: Array[Int], iters : Int): KMeansModel = {

    // The clusters' centroids are moved.
    val result = assignToClusters(dataSet, _initialModel.map(_.moveCenter), assignments)
    // The updated model.
    val newClusters = result._1
    //The number of assignments
    val numberOfAssignments = result._2

    // Stop condition of the algorithm
    if( iters >= maxIters || numberOfAssignments == 0)
      newClusters
    else
      iterate(dataSet, newClusters,assignments,iters + 1)
  }

  /**
    * Private method that assigns each instance of the dataSet to it nearest cluster.
    * @param dataSet the dataSet that is going to be used to learn the clusters.
    * @param clusters current model.
    * @param assignments the array containing the assigned cluster for each data instance.
    * @return the updated model and the number of assignments that have been made.
    */
  // TODO: (Borrar comment???) Aqui esta reasignando sin tener en cuenta si esa instancia ya pertenecia a dicho cluster
  private def assignToClusters(dataSet: DataSet, clusters: KMeansModel, assignments: Array[Int]): (KMeansModel, Int) ={

    var numberOfAssignments = 0

    for(instanceIndex <- dataSet.data.indices) {
      val nearestCluster = getNearestCluster(clusters, dataSet.data(instanceIndex))
      // re-assign if the observations does not belong to this nearest cluster
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
    * @param clusters current model.
    * @param instance the data instance to be assigned.
    * @return the nearest cluster's index.
    */
  private def getNearestCluster(clusters: KMeansModel, instance: Array[Double]): Int ={

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

object KMeans{

  /** Simply defines a List of clusters as a model. */
  type KMeansModel = List[KMeansCluster]
}


