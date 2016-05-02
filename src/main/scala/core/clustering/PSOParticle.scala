package core.clustering

import core.DataSet

import scala.util.Random

// Esta clase es la que se encarga automaticamente (simplemente pasandole el dataset)
// de generar los máximos y mínimos para cada dimensión (de tal forma que la particula no se salga del plano)
// y de asignar una velocidad máxima y minima
case class PSOParticleConfig(dataSet: DataSet,
                             maxVel: Int = 1,
                             minVel: Int = -1){
  val maxDimensions = {
    //Ineficiente a causa del transpose
    dataSet.data.transpose.map(x => x.max)
  }

  val minDimensions = {
    //Ineficiente a causa del transpose
    dataSet.data.transpose.map(x => x.min)
  }
}


class PSOParticle (velocity: Array[Double],
                   location: Array[Double]) {



}

object PSOParticle{
  //Seleccionamos los atributos de una instancia aleatoria del dataset y la usamos como centroide

  // en vez de pasarle una referencia del dataset, le pasamos un objeto de configuración
  // que se aplicará a TODAS las partículas
  def initialize(dataSet:DataSet): PSOParticle = {
    //Seleccionamos los atributos de una instancia aleatoria del dataset y la usamos como centroide
    //de la particula
    val indexes = collection.mutable.Set(Random.nextInt(dataSet.data.length))

    //TODO
    null
  }
}