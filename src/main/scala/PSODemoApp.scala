import core.clustering.pso.{PSOParticleConfig, PSO}
import core.util.Distances
import core.{DataSourceConfig, DataSource}

/**
  * Created by Fer on 10/05/2016.
  */
object PSODemoApp {

  def main(args: Array[String]): Unit = {
    println("DemoApp esta en marcha")

    //irisTest()
    diabetesTest()
  }

  // Checked dims [0,1] [1,2]
  private def irisTest(): Unit ={
    val url = System.getProperty("user.dir") + "\\data\\iris-modified.csv"
    val dataSource = DataSource(DataSourceConfig(url, normalize = true))
    //Devuelve un Try, asi que no valdria con eso, habria que comprobar que ha sido un exito
    val dataSet = dataSource.generateDataSet()

    println("Dataset generado")

    //                                          Dataset,       distancia,                    c1, c2, c3
    val particleConfig = new PSOParticleConfig(dataSet,Distances.Euclidean[Double, Double], 0.1, 1, 0.5 )
    // config, N clusters, N iteraciones,
    val psoClusteringAlgorithm = new PSO(particleConfig, 3, 10, 3, Distances.Euclidean[Double, Double])

    println("PSO algorithm created")

    // http://stackoverflow.com/questions/25593567/scala-throw-error-vs-return-try
    val initialMoment = System.currentTimeMillis()
    val obtainedModel = psoClusteringAlgorithm.train(dataSet)
    val finalMoment = System.currentTimeMillis()

    println("Inter-cluster distance: " + obtainedModel.evaluateInterClusterDistances(Distances.Euclidean[Double, Double]))
    println("Intra-cluster distance: " + obtainedModel.evaluateIntraClusterDistances(Distances.Euclidean[Double, Double]))
    println("Training time: "+ (finalMoment - initialMoment))

    val stop = false
    while(!stop){

      val ln = readLine("Dimension 1: ")
      val ln2 = readLine("Dimension 2: ")

      println("Dimensions ["+ ln.toInt + ","+ln2.toInt+"]")
      obtainedModel.render2D("PSO",ln.toInt,ln2.toInt)
    }
  }
  // Checked [1,5] se ven los centroides
  // Checked [5,7] se ve algo los centroids
  private def diabetesTest(): Unit ={

    val url = System.getProperty("user.dir") + "\\data\\diabetes-modified.csv"
    val dataSource = DataSource(DataSourceConfig(url, normalize = true))
    //Devuelve un Try, asi que no valdria con eso, habria que comprobar que ha sido un exito
    val dataSet = dataSource.generateDataSet()

    println("Dataset generado")
    //                                          Dataset,       distancia,                    c1, c2, c3
    val particleConfig = new PSOParticleConfig(dataSet,Distances.Euclidean[Double, Double], 0.1, 1, 0.5)
    //
    val psoClusteringAlgorithm = new PSO(particleConfig, 3, 10, 3, Distances.Euclidean[Double, Double])

    println("PSO algorithm created")

    // http://stackoverflow.com/questions/25593567/scala-throw-error-vs-return-try
    val initialMoment = System.currentTimeMillis()
    val obtainedModel = psoClusteringAlgorithm.train(dataSet)
    val finalMoment = System.currentTimeMillis()

    println("Inter-cluster distance: " + obtainedModel.evaluateInterClusterDistances(Distances.Euclidean[Double, Double]))
    println("Intra-cluster distance: " + obtainedModel.evaluateIntraClusterDistances(Distances.Euclidean[Double, Double]))
    println("Training time: "+ (finalMoment - initialMoment))

    val stop = false
    while(!stop){

      val ln = readLine("Dimension 1: ")
      val ln2 = readLine("Dimension 2: ")

      println("Dimensions ["+ ln.toInt + ","+ln2.toInt+"]")
      obtainedModel.render2D("PSO",ln.toInt,ln2.toInt)
    }
  }
}
