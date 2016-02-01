package core.util

/**
 * Created by Fernando on 31/01/2016.
 */
class Statistics(values: Seq[Seq[Double]]) {

  lazy val sums:Seq[Double] = values.transpose.map(_.sum)

  lazy val means:Seq[Double] = sums.map(_ / values.size)

  //Cada dimension menos su media todo ello al cuadrado y dividido por el numero de elementos
  lazy val variance:Seq[Double] = {
    for(value <- values)
      yield value.map(Math.pow(_,2) / values.size)//N esta temrinado, quizas hay que hacer un fold, no se
  }

  lazy val standardDeviation:Seq[Double] = variance.map(Math.sqrt(_))

}
