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
                            private var position: PositionDefinition) {

  private var inertia = 0

  private var fitnessValue = Double.MaxValue

  private var bestFitnessValue = Double.MaxValue

  private var bestPosition: List[Array[Double]] = position.getValue

  def getVelocity = this.velocity

  def getPosition = this.position

  def getFitnessValue = this.fitnessValue

  // The lower the better, max = 0
  def calculateFitnessValue: Double = {

  this.fitnessValue = position.clusters
    .map(x => x.distancesSum(config.distanceFunc) / x.size) // Cluster "score"
    .sum / position.clusters.size // Score_Sum divided by the number of clusters

  this.fitnessValue
  }

  // Si queremos actualizar tmb la inercia, seria necesario pasar el numero de iteraciones o algun factor
  // sobre el que multiplicar la inercia

  //este metodo se llamara en la parte de recursion del PSO
  def update(globalBestPosition: List[Array[Double]]): Unit = {

    /** New velocity */

    // Inertia
    val firstTerm: List[Array[Double]] = for(vector <- velocity) yield vector.map(_ * this.inertia)
    // Previous best
    val r1 = for{i <- this.bestPosition.indices}
      yield this.bestPosition(i).zip(position.getValue(i)).map{case (x,y) => x - y}
    val secondTerm = for(vector <- r1) yield vector.map(_ * config.c1)
    // Global best
    val r2 = for{ i <- globalBestPosition.indices}
      yield globalBestPosition(i).zip(position.getValue(i)).map{case (x,y) => x - y}
    val globalTerm = for(vector <- r2) yield vector.map(_ * config.c2)

    val newVelocity = for{i <- globalBestPosition.indices}
      yield (firstTerm(i), secondTerm(i), globalTerm(i)).zipped.map(_ + _ + _)

    /** New position */

    // Modify the centroid values of the clusters contained in the particle
    val newCentroidValues  = for( i <- newVelocity.indices)
      yield newVelocity(i).zip(position.getValue(i)).map{case (x,y) => x + y}

    /** Checking the max/min dimensions of the velocity and position, if they overcome its limits, they will be overwritten */

    val checkedVelocity = checkVelocity(newVelocity.toList)
    val checkedCentroidValues = checkPosition(newCentroidValues.toList)

    /** Update the particle with its new velocity and position */
    // Position
    checkedCentroidValues.zip(position.clusters).map{case (newCentroid, cluster) => cluster.moveCenter(newCentroid)}
    // Velocity
    this.velocity = checkedVelocity

    // The new and best fitness values are compared
    val bestFitnessValue = this.bestFitnessValue
    val newFitnessValue = calculateFitnessValue

    if(bestFitnessValue > newFitnessValue){
      // The Best Personal location is updated
      this.bestPosition = position.getValue
      this.bestFitnessValue = newFitnessValue
    }

  }

  private def checkVelocity(velocity: List[Array[Double]]): List[Array[Double]] = {

    for(centroidVelocity <- velocity)
      yield centroidVelocity.map(dimension => {
        if (dimension > config.maxVel)
          config.maxVel
        else if (dimension < config.minVel)
          config.minVel
        else dimension
      })
  }

  private def checkPosition(position:  List[Array[Double]]): List[Array[Double]] = {

    // Check Max dimension
    val maxChecked = for(centroidPosition <- position)
      yield centroidPosition
        .zip(config.maxPosition)
        .map{case (x,y) =>{
          if(x > y) y
          else x
        }}

    // Check Min dimension
    for(centroidPosition <- maxChecked)
      yield centroidPosition
        .zip(config.minPosition)
        .map{case (x,y) =>{
          if(x < y) y
          else x
        }}
  }

}


object PSOParticle{

  // Public constructor
  def apply(config: PSOParticleConfig,dataSet: DataSet, nClusters: Int): PSOParticle = {

    /** Position */

    val indexes = collection.mutable.Set(Random.nextInt(dataSet.data.length))
    while (indexes.size < nClusters)
      indexes.add(Random.nextInt(dataSet.data.length))
    val initialClusters = indexes.toList.map(x => new CentroidCluster(dataSet.data(x),dataSet))

    /** Velocity */

    // Initially, all the centroids of the particle will have the same velocity
    val velocity = for( i <- 1 to dataSet.instanceSize)
      yield ThreadLocalRandom.current().nextDouble(config.minVel,config.maxVel)

    new PSOParticle(config, List.fill(nClusters)(velocity.toArray), new PositionDefinition(Array.fill(dataSet.data.length)(-1),initialClusters))
  }

}

// Esta clase es la que se encarga automaticamente (simplemente pasandole el dataset)
// de generar los máximos y mínimos para cada dimensión (de tal forma que la particula no se salga del plano)
// y de asignar una velocidad máxima y minima
case class PSOParticleConfig(dataSet: DataSet,
                             distanceFunc: DistanceFunc,
                             maxVel: Double = 1.0,
                             minVel: Double = -1.0,
                             inertiaUpperBound: Double = 1.0,
                             inertiaLowerBound: Double = 0.0,
                             c1: Double = 2.0,
                             c2: Double = 2.0){

  final val maxPosition = dataSet.data.transpose.map(x => x.max)
  final val minPosition = dataSet.data.transpose.map(x => x.min)

}

case class PositionDefinition(assignments: Array[Int], clusters: List[CentroidCluster]){

  def getValue: List[Array[Double]] = {
    clusters.map(_.getCentroid)
  }
}