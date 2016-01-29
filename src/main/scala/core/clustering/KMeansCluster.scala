package core.clustering

import core.{DataSet, Observation}

class KMeansCluster(dataSet: DataSet) {

  class Centroid(observation: Observation){

  }

  lazy val centroids: Array[Centroid] = generateCentroids()

  private def generateCentroids() ={
    null
  }
}
