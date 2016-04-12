package core.clustering

import core.DataSet
import core.util.Distances._

import scala.util.{Failure, Success, Try}


case class PSOConfig(K: Int,
                     maxIters: Int,
                     swarmSize: Int = 10,
                     inertiaUpperBound: Double = 1.0,
                     inertiaLowerBound: Double = 0.0,
                     c1: Double = 2.0,
                     c2: Double = 2.0)

class PSO (config: PSOConfig,
           distance: DistanceFunc,
           dataSet:DataSet) {

  def train: Option[KMeansModel]= Try{

    // inicializamos el swarm
    // actualizamos la funcion de fitness
    // tailrecusive iteration

  }match {
    case Success(clusters) => Some(clusters)
    case Failure(exception) => None
  }

  def initializeSwarm: List[PSOParticle] = {

    val particleConfig = PSOParticleConfig(dataSet)
    //Creamos partículas hasta el número máximo especificado
    for(i <- config.swarmSize)
      PSOParticle.initialize(dataSet)

  }

}
