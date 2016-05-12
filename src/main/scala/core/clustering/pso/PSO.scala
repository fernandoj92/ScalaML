package core.clustering.pso

import core.DataSet
import core.clustering.model.{CentroidCluster, CentroidModel}
import core.util.Distances._

import scala.annotation.tailrec



class PSO (particleConfig: PSOParticleConfig,
           K: Int,
           maxIters: Int,
           swarmSize: Int = 10,
           distance: DistanceFunc) {

  def train(dataSet: DataSet): CentroidModel ={
    require(dataSet.data.length >= K,
      s"K ($K) cannot be greater than the dataSet size (${dataSet.data.length})")
    // inicializamos el swarm con valores aleatorios de velocidad y posicion
    val initialSwarm = initializeSwarm(particleConfig, dataSet)

    // Asignamos las instancias a cada uno de los clusters pertenecientes a cada una de las particulas
    for(particle <- initialSwarm)
      CentroidCluster.assignToClusters(dataSet, particle.getPosition.clusters, particle.getPosition.assignments, distance)

    // Calculamos las fitness functions de cada particula
    for(particle <- initialSwarm)
      particle.calculateFitnessValue

    // Ahora ya tendriamos las instancias asignadas, yo creo que habria que comenzar la recursión
    val iterations = 1

    val bestClusters = iterate(dataSet, initialSwarm, iterations)

    new CentroidModel(bestClusters._1,bestClusters._2, dataSet)

  }

  private def initializeSwarm(particleConfig: PSOParticleConfig, dataSet: DataSet): List[PSOParticle] = {
    val particleSwarm = for(i <- 0 until swarmSize)
      yield PSOParticle(particleConfig, dataSet, K)

    particleSwarm.toList
  }

  @tailrec
  private def iterate(dataSet: DataSet, particles: List[PSOParticle], iters: Int): (List[CentroidCluster], Array[Int]) ={
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
      // The cluster and the array containing current assignments are returned
      (bestParticle.getPosition.clusters, bestParticle.getPosition.assignments)
    }else
      iterate(dataSet, particles, iters + 1)
  }

}
