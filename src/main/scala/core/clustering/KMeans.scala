package core.clustering

import core.DataSet
import core.clustering.KMeans.KMeansModel
import core.util.Distances.DistanceFunc


case class KMeansConfig(K: Int, maxIters: Int)

class KMeans (config: KMeansConfig,
              distance: DistanceFunc,
              dataSet:DataSet) {

  def train: KMeansModel ={
    require( dataSet.data.length >= config.K,
      s"K ($config.K)cannot be greater than the dataSet size (${dataSet.data.length})")

    //Metodo principal de la clase, principalmente se va a encargar de generar los centroides y con ellos
    // los clusters, utilizando las distancias
  }

}

object KMeans{

  type KMeansModel = List[KMeansCluster]
}


