package core.clustering.pso

import core.DataSet
import core.clustering.model.{CentroidCluster, CentroidModel}
import core.util.Distances._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}



class PSO (K: Int,
           maxIters: Int,
           swarmSize: Int = 10,
           distance: DistanceFunc) {

  def train(dataSet: DataSet): Option[CentroidModel]= Try{

    // inicializamos el swarm con valores aleatorios de velocidad y posicion
    val initialSwarm = initializeSwarm(dataSet)

    // Asignamos las instancias a cada uno de los clusters pertenecientes a cada una de las particulas
    for(particle <- initialSwarm)
      CentroidCluster.assignToClusters(dataSet, particle.getPosition.clusters, particle.getPosition.assignments, distance)

    // Calculamos las fitness functions de cada particula
    for(particle <- initialSwarm)
      particle.calculateFitnessValue

    // Ahora ya tendriamos las instancias asignadas, yo creo que habria que comenzar la recursión
    val iterations = 1

    val bestClusters = iterate(dataSet, initialSwarm, iterations)

    new CentroidModel(bestClusters, dataSet)

  }match {
    case Success(clusters) => Some(clusters)
    case Failure(exception) => None
  }

  private def initializeSwarm(dataSet: DataSet): List[PSOParticle] = {
    val particleConfig = PSOParticleConfig(dataSet, distance)
    val particleSwarm = for(i <- 0 until swarmSize)
      yield PSOParticle(particleConfig, dataSet, K)

    particleSwarm.toList
  }

  @tailrec
  private def iterate(dataSet: DataSet, particles: List[PSOParticle], iters: Int): List[CentroidCluster] ={
    // Iteramos por la lista de particulas y obtenemos la mejor de todas
    val bestParticleIndex = particles.map(_.getFitnessValue).zipWithIndex.minBy(_._1)._2
    val bestParticle = particles(bestParticleIndex)

    // Actualizamos la fitness function de cada particula, junto con su velocidad y posicion
    particles.foreach(_.update(bestParticle.getPosition.getValue))

    // Asignamos las instancias a cada uno de los clusters pertenecientes a cada una de las particulas
    for(particle <- particles)
      CentroidCluster.assignToClusters(dataSet, particle.getPosition.clusters, particle.getPosition.assignments, distance)

    // Stop condition of the algorithm
    if( iters >= maxIters ){
      // The best particle's clusters are returned
      val bestParticleIndex = particles.map(_.getFitnessValue).zipWithIndex.minBy(_._1)._2
      val bestParticle = particles(bestParticleIndex)
      bestParticle.getPosition.clusters
    }else
      iterate(dataSet, particles, iters + 1)
  }

}
