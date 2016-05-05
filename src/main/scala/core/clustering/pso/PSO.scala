package core.clustering.pso

import core.DataSet
import core.clustering.model.{CentroidCluster, CentroidModel}
import core.util.Distances
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

  def train: Option[CentroidModel]= Try{

    // inicializamos el swarm
    val initialSwarm = initializeSwarm

    // actualizamos la funcion de fitness


    // tailrecusive iteration
    // TODO
  new CentroidModel(null, null)
  }match {
    case Success(clusters) => Some(clusters)
    case Failure(exception) => None
  }

  private def initializeSwarm: List[PSOParticle] = {

    val particleConfig = PSOParticleConfig(dataSet)

    val particleSwarm = for(i <- 0 until config.swarmSize)
      yield PSOParticle(particleConfig, dataSet, config.K)

    particleSwarm.toList
  }
  //Se actualiza la velocidad y la posicion de cada particula teniendo en cuenta las velocidades globales y locales
  private def updateFitnessFunction(particles: List[PSOParticle]): Double = {

    // Iteramos por la lista de particulas y por cada una de ellas
    val fitnessValues =
      for {
        particle <- particles
        // Calculamos la distancia de cada instancia con respecto de cada centroide contenido en la particula y asignamos las instancias
        CentroidCluster.assignToClusters(dataSet, particle.getClusters, particle.getCurrentAssignments, Distances.Euclidean[Double, Double])
        // Calculamos la funcion de fitness de dicha particula
    } yield particle.calculateFitnessValue

    0
  }

}
