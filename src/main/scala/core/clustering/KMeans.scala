package core.clustering

import core.DataSet
import core.clustering.KMeans.KMeansModel
import core.util.Distances.DistanceFunc

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try, Random}


case class KMeansConfig(K: Int, maxIters: Int){
  require(maxIters > 0,
    s"The maximum number of iterations ($maxIters) cannot be lesser than 1")
}

class KMeans (config: KMeansConfig,
              distance: DistanceFunc[Double, Double],
              dataSet:DataSet) {

  def train: Option[KMeansModel] = Try{
    require(dataSet.data.length >= config.K,
      s"K ($config.K) cannot be greater than the dataSet size (${dataSet.data.length})")

    //Initialization
    val initialModel = initialize //Al inicializar damos valor a los centroides
    val assignments = Array.fill(dataSet.data.length)(0) //Utilizamos un array complementario para recordar el cluster al que pertenece cada miembro
    val assignedModel = assignToClusters(initialModel, assignments)._1 //Asignamos las instancias
    val iterations = 0

    // Launch the recursion
    iterate(assignedModel, assignments, iterations)

  }match {
    case Success(clusters) => Some(clusters)
    case Failure(exception) => None
  }

  // Random initialization of the centroids
  private def initialize: KMeansModel = {
    val indexes = collection.mutable.Set(Random.nextInt(dataSet.data.length))
    while (indexes.size < config.K)
      indexes.add(Random.nextInt(dataSet.data.length))
    // It is necessary to transform it at the beginning or we would ignore 'repeated' instances
    indexes.toList.map(x => KMeansCluster(dataSet.data(x),dataSet))
  }

  // Es necesario modificar los metodos para poder tener en cuenta si no se han producido reasignaciones,
  // ya que es una condición de parada
  // Tambien es necesario mover el centroide
  // AL SER RECURSIVO NO ES NECESARIO GUARDAR LOS CLUSTERS ya que los iremos pasando
  @tailrec
  private def iterate(_initialModel: KMeansModel, assignments: Array[Int], iters : Int): KMeansModel = {
    //Movemos el centroide
    val f = assignToClusters(_initialModel.map(_.moveCenter), assignments)
    val newClusters = f._1
    val numberOfAssignments = f._2

    if( iters >= config.maxIters || numberOfAssignments == 0)
      newClusters
    else
      iterate(newClusters,assignments,iters + 1)
  }
  // Aqui esta reasignando sin tener en cuenta si esa instancia ya pertenecia a dicho cluster
  private def assignToClusters(clusters: KMeansModel, assignments: Array[Int]): (KMeansModel, Int) ={
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
    (clusters,numberOfAssignments)
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


