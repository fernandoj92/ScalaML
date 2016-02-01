package core.clustering

import core.DataSet
import core.clustering.KMeans.KMeansModel
import core.util.Distances.DistanceFunc

import scala.util.Random


case class KMeansConfig(K: Int, maxIters: Int)

class KMeans (config: KMeansConfig,
              distance: DistanceFunc,
              dataSet:DataSet) {

  def train: KMeansModel ={
    require( dataSet.data.length >= config.K,
      s"K ($config.K)cannot be greater than the dataSet size (${dataSet.data.length})")

    //Metodo principal de la clase, principalmente se va a encargar de generar los centroides y con ellos
    // los clusters, utilizando las distancias

    //Inicialización

  }

  // En principio los generamos aleatoriamente, luego cuando se hayan asignado todas las observaciones a un cluster u otro
  // haremos una  recolocación
  private def initialize: KMeansModel ={
    val indexes =  collection.mutable.Set(Random.nextInt(dataSet.data.length))
    while(indexes.size < config.K)
      indexes.add(Random.nextInt(dataSet.data.length))


  }

}

object KMeans{

  type KMeansModel = List[KMeansCluster]
}


