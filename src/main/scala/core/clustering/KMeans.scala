package core.clustering

import core.DataSet
import core.clustering.KMeans.KMeansModel
import core.util.Distances.DistanceFunc

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try, Random}


case class KMeansConfig(K: Int, maxIters: Int)

class KMeans (config: KMeansConfig,
              distance: DistanceFunc[Double, Double],
              dataSet:DataSet) {

  def train: Option[KMeansModel] = Try{
    require(dataSet.data.length >= config.K,
      s"K ($config.K)cannot be greater than the dataSet size (${dataSet.data.length})")

    //Initialization
    val initialModel = initialize
    val assignedModel = assignToClusters(initialModel)



  }match {
    case Success(clusters) => Some(clusters)
    case Failure(exception) => None
  }

  // Random initialization
  private def initialize: KMeansModel = {
    val indexes = collection.mutable.Set(Random.nextInt(dataSet.data.length))
    while (indexes.size < config.K)
      indexes.add(Random.nextInt(dataSet.data.length))
    // We have to transform it at the beginning or we would ignore repeated instances
    indexes.toList.map(x => KMeansCluster(dataSet.data(x),dataSet))
  }

  @tailrec
  private def iterate: KMeansModel = {

  }

  private def assignToClusters(clusters: KMeansModel): KMeansModel ={
    for(instance <- dataSet.data.indices){
      val nearestCluster = getNearestCluster(clusters,dataSet.data(instance))
      clusters(nearestCluster) += instance
    }
    clusters
  }

  private def getNearestCluster(clusters: KMeansModel, instance: Array[Double]): Int ={
    val distances = clusters.zipWithIndex.map{
      case (cluster,index) => (distance(instance,cluster.getCentroid),index)
    }
    val min = distances.minBy(_._1)
    val nearestCluster = min._2
    nearestCluster
  }

}

object KMeans{

  type KMeansModel = List[KMeansCluster]
}


