package core.clustering.pso

import java.util.concurrent.ThreadLocalRandom

import core.DataSet
import core.clustering.model.CentroidCluster

import scala.util.Random

// Private constructor
// Each particle position is equal to a Cluster solution
class PSOParticle private(config: PSOParticleConfig,
                          dataSet: DataSet,
                          velocity: Array[Double],
                          position: PositionDefinition) {

  def getClusters = position.clusters
  def getCurrentAssignments = position.assignments
  def calculateFitnessValue: Double = {
    for(cluster <- position.clusters){
      cluster.distancesSum()
    }

  }

}

object PSOParticle{

  // Public constructor
  def apply(config: PSOParticleConfig,dataSet: DataSet, nClusters: Int): PSOParticle = {
    // Location
    val indexes = collection.mutable.Set(Random.nextInt(dataSet.data.length))
    while (indexes.size < nClusters)
      indexes.add(Random.nextInt(dataSet.data.length))
    val initialPosition = indexes.toList.map(x => new CentroidCluster(dataSet.data(x),dataSet))

    //
    val velocity = for( i <- 1 to dataSet.instanceSize)
      yield ThreadLocalRandom.current().nextDouble(config.minVel,config.maxVel)

    new PSOParticle(config, dataSet, velocity.toArray, new PositionDefinition(Array.fill(dataSet.data.length)(-1),initialPosition))
  }
}

// Esta clase es la que se encarga automaticamente (simplemente pasandole el dataset)
// de generar los máximos y mínimos para cada dimensión (de tal forma que la particula no se salga del plano)
// y de asignar una velocidad máxima y minima
case class PSOParticleConfig(dataSet: DataSet,
                             maxVel: Int = 1,
                             minVel: Int = -1){

  final val maxDimensions = dataSet.data.transpose.map(x => x.max)
  final val minDimensions = dataSet.data.transpose.map(x => x.min)

}

private case class PositionDefinition(assignments: Array[Int], clusters: List[CentroidCluster])
