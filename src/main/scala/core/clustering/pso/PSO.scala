package core.clustering.pso

import core.DataSet
import core.clustering.model.{CentroidCluster, CentroidModel}
import core.util.Distances
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
      CentroidCluster.assignToClusters(dataSet, particle.position.clusters, particle.position.assignments, distance)

    // Calculamos las fitness functions de cada particula
    for(particle <- initialSwarm)
      particle.calculateFitnessValue

    // Ahora ya tendriamos las instancias asignadas, yo creo que habria que comenzar la recursión

    // tailrecusive iteration
    // TODO
    new CentroidModel(null, null)

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

  private def iterate(dataSet: DataSet, particles: List[PSOParticle], iters: Int): List[PSOParticle] ={
    // Iteramos por la lista de particulas y obtenemos la mejor de todas
    val bestParticleIndex = particles.map(_.getFitnessValue).zipWithIndex.minBy(_._1)._2
    val bestParticle = particles(bestParticleIndex)

    // Actualizamos la fitness function de cada particula, junto con su velocidad y posicion
    val newSwarm = particles.map(_.update(bestParticle.position.getValue))

    // Asignamos las instancias a cada uno de los clusters pertenecientes a cada una de las particulas
    for(particle <- newSwarm)
      CentroidCluster.assignToClusters(dataSet, particle.position.clusters, particle.position.assignments, distance)

    // Stop condition of the algorithm
    if( iters >= maxIters)
      null
    null
  }


  /*
  private def calculateFitnessFunction(particles: List[PSOParticle]): Seq[Double] = {

    // Iteramos por la lista de particulas y por cada una de ellas
      for {
        particle <- particles
        // Calculamos la distancia de cada instancia con respecto de cada centroide contenido en la particula y asignamos las instancias
        CentroidCluster.assignToClusters(dataSet, particle.getClusters, particle.getCurrentAssignments, Distances.Euclidean[Double, Double])
      // Calculamos la funcion de fitness de dicha particula
      } yield particle.calculateFitnessValue(Distances.Euclidean[Double, Double])

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
    } yield particle.calculateFitnessValue(Distances.Euclidean[Double, Double])

    val bestParticleIndex = fitnessValues.zipWithIndex.minBy(_._1)._2
    val bestParticle = particles(bestParticleIndex)

    particles.foreach(_.update(bestParticle.position.getValue))


    0
  }
*/
}
