package core.clustering.pso

import java.util.concurrent.ThreadLocalRandom

import core.DataSet
import core.clustering.model.CentroidCluster
import core.util.Distances.DistanceFunc

import scala.util.Random

// Private constructor
// Each particle position is equal to a Clustering solution
class PSOParticle  private (config: PSOParticleConfig,
                            private var velocity: List[Array[Double]],
                            val position: PositionDefinition) {

  private var inertia = 0

  private var fitnessValue = Double.MaxValue

  private var bestPosition: List[Array[Double]] = position.getValue

  def getClusters = position.clusters

  def getCurrentAssignments = position.assignments
  // The lower the better, max = 0
  def calculateFitnessValue (distanceFunc: DistanceFunc): Unit =
    fitnessValue = position.clusters
      .map(x => x.distancesSum(distanceFunc) / x.size) // Cluster "score"
      .sum / position.clusters.size                    // Score_Sum divided by the number of clusters

  def getFitnessValue = fitnessValue

  // Si queremos actualizar tmb la inercia, seria necesario pasar el numero de iteraciones o algun factor
  // sobre el que multiplicar la inercia

  //este metodo se llamara en la parte de recursion del PSO
  def update(globalBestPosition: List[Array[Double]]): Unit ={

    /* New velocity */

    // Inertia
    val firstTerm: List[Array[Double]] = for(vector <- velocity) yield vector.map(_ * inertia)
    // Previous best
    val r1 = for{i <- bestPosition.indices}
      yield bestPosition(i).zip(position.getValue(i)).map{case (x,y) => x - y}
    val secondTerm = for(vector <- r1) yield vector.map(_ * config.c1)
    // Global best
    val r2 = for{ i <- globalBestPosition.indices}
      yield globalBestPosition(i).zip(position.getValue(i)).map{case (x,y) => x - y}
    val globalTerm = for(vector <- r2) yield vector.map(_ * config.c2)

    val newVelocity = for{i <- globalBestPosition.indices}
      yield (firstTerm(i), secondTerm(i), globalTerm(i)).zipped.map(_ + _ + _)

    velocity = newVelocity.toList

    /* New position */

    // Modify the centroid values of the clusters contained in the particle
    newVelocity.flatMap(x => position.clusters.map(_.moveCenter(x)))

    // Aqui yo creo que habria que modificar la bestLocalPosition
    if(bestPosition)
  }
}

object PSOParticle{

  // Public constructor
  def apply(config: PSOParticleConfig,dataSet: DataSet, nClusters: Int): PSOParticle = {

    /* Position */

    val indexes = collection.mutable.Set(Random.nextInt(dataSet.data.length))
    while (indexes.size < nClusters)
      indexes.add(Random.nextInt(dataSet.data.length))
    val initialPosition = indexes.toList.map(x => new CentroidCluster(dataSet.data(x),dataSet))

    /* Velocity */

    // Initially, all the centroids of the particle will have the same velocity
    val velocity = for( i <- 1 to dataSet.instanceSize)
      yield ThreadLocalRandom.current().nextDouble(config.minVel,config.maxVel)

    new PSOParticle(config, List.fill(nClusters)(velocity.toArray), new PositionDefinition(Array.fill(dataSet.data.length)(-1),initialPosition))
  }
}

// Esta clase es la que se encarga automaticamente (simplemente pasandole el dataset)
// de generar los máximos y mínimos para cada dimensión (de tal forma que la particula no se salga del plano)
// y de asignar una velocidad máxima y minima
case class PSOParticleConfig(dataSet: DataSet,
                             maxVel: Int = 1,
                             minVel: Int = -1,
                             inertiaUpperBound: Double = 1.0,
                             inertiaLowerBound: Double = 0.0,
                             c1: Double = 2.0,
                             c2: Double = 2.0){

  final val maxDimensions = dataSet.data.transpose.map(x => x.max)
  final val minDimensions = dataSet.data.transpose.map(x => x.min)

}

private case class PositionDefinition(assignments: Array[Int], clusters: List[CentroidCluster]){

  def getValue: List[Array[Double]] = {
    clusters.map(_.getCentroid)
  }
}

