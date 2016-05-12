package core.clustering.hybrid

import core.DataSet
import core.clustering.kmeans.KMeans
import core.clustering.model.CentroidModel
import core.clustering.pso.PSO

/**
  * Created by Fernando on 5/12/2016.
  */
class HybridPSOKMeans (kmeans: KMeans,
                       pso: PSO){

  def train(dataSet: DataSet): CentroidModel ={

    val psoModel = pso.train(dataSet)
    kmeans.train(psoModel,dataSet)

  }

}
