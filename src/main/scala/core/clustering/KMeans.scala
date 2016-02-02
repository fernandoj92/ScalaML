package core.clustering

import core.DataSet
import core.clustering.KMeans.KMeansModel
import core.util.Distances.DistanceFunc

import scala.util.{Failure, Success, Try, Random}


case class KMeansConfig(K: Int, maxIters: Int)

class KMeans (config: KMeansConfig,
              distance: DistanceFunc[Double, Double],
              dataSet:DataSet) {

  def train: Option[KMeansModel] = Try{
    require(dataSet.data.length >= config.K,
      s"K ($config.K)cannot be greater than the dataSet size (${dataSet.data.length})")

    //Metodo principal de la clase, principalmente se va a encargar de generar los centroides y con ellos
    // los clusters, utilizando las distancias

    //Inicialización
    val initialModel = initialize
    initialModel

  }match {
    case Success(clusters) => Some(clusters)
    case Failure(exception) => None
  }

  // En principio los generamos aleatoriamente, luego cuando se hayan asignado todas las observaciones a un cluster u otro
  // haremos una  recolocación
  private def initialize: KMeansModel = {
    val indexes = collection.mutable.Set(Random.nextInt(dataSet.data.length))
    while (indexes.size < config.K)
      indexes.add(Random.nextInt(dataSet.data.length))
    // Si transformamos al final podria darse el caso de que hayamos ignorado valores iguales (no nos interesa ignorarlos)
    indexes.toList.map(x => KMeansCluster(dataSet.data(x),dataSet))
  }

  private def iterate: KMeansModel = {

  }

  // Este método es el que define la comparación de cada instancia con los centroides
  // y asigna la instancia al mas cercano
  private def assignToCluster(clusters: KMeansModel): KMeansModel ={
    // Iteramos por las instancias del dataSet
    for(instanciaID <- dataSet.data.indices) {
      // Iteramos por los clusters y comparamos la distancia entre el centroide del cluster y la instancia
      for(clusterID <- clusters.indices){
        val distancia = distance(dataSet.data(instanciaID), clusters(clusterID).getCentroid)
      }
    }
    // Almacenamos el ID del cluster con la distancia minima

    // Asignamos el ID de la instancia al cluster
  }

  private def getNearestCluster(clusters: KMeansModel, instance: Array[Double]): Int ={
    /*clusters.view.zipWithIndex.foreach{
      case (cluster, index) =>
        val measure = distance(instance, cluster.getCentroid)
    }*/
    // Un enfoque interesante puede ser crear una nueva lista de tuplas con el indice y la distancia
    // y luego ya pasamos y escogemos el minimo
    clusters.view.zipWithIndex.foldLeft{
      (cluster, index) =>
    }
  }

}

object KMeans{

  type KMeansModel = List[KMeansCluster]
}


