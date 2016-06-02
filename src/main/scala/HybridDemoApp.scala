import core.clustering.pso.{PSO, PSOParticleConfig}
import core.util.Distances
import core.{DataSourceConfig, DataSource}

/**
  * Created by Fernando on 5/12/2016.
  */
object HybridDemoApp {

  def main(args: Array[String]): Unit = {
    println("DemoApp esta en marcha")

    irisTest()
    //diabetesTest()
  }

  private def irisTest(): Unit ={
    val url = System.getProperty("user.dir") + "\\data\\iris-modified.csv"
    val dataSource = DataSource(DataSourceConfig(url, normalize = true))
    //Devuelve un Try, asi que no valdria con eso, habria que comprobar que ha sido un exito
    val dataSet = dataSource.generateDataSet()

    println("Dataset generado")

    val particleConfig = new PSOParticleConfig(dataSet,Distances.Euclidean[Double, Double], 0.1, 1, 0.5 )
    val psoClusteringAlgorithm = new PSO(particleConfig, 3, 10, 3, Distances.Euclidean[Double, Double])

    println("PSO algorithm created")

    // http://stackoverflow.com/questions/25593567/scala-throw-error-vs-return-try
    val initialMoment = System.currentTimeMillis()
    val obtainedModel = psoClusteringAlgorithm.train(dataSet)
    val finalMoment = System.currentTimeMillis()

    println("Inter-cluster distance: " + obtainedModel.evaluateInterClusterDistances(Distances.Euclidean[Double, Double]))
    println("Intra-cluster distance: " + obtainedModel.evaluateIntraClusterDistances(Distances.Euclidean[Double, Double]))
    println("Training time: "+ (finalMoment - initialMoment))

    obtainedModel.render2D("PSO",0,1)
  }

  private def diabetesTest(): Unit ={

    val url = System.getProperty("user.dir") + "\\data\\diabetes-modified.csv"
    val dataSource = DataSource(DataSourceConfig(url, normalize = true))
    //Devuelve un Try, asi que no valdria con eso, habria que comprobar que ha sido un exito
    val dataSet = dataSource.generateDataSet()

    println("Dataset generado")

    val particleConfig = new PSOParticleConfig(dataSet,Distances.Euclidean[Double, Double] )
    val psoClusteringAlgorithm = new PSO(particleConfig, 3, 10, 3, Distances.Euclidean[Double, Double])

    println("PSO algorithm created")

    // http://stackoverflow.com/questions/25593567/scala-throw-error-vs-return-try
    val initialMoment = System.currentTimeMillis()
    val obtainedModel = psoClusteringAlgorithm.train(dataSet)
    val finalMoment = System.currentTimeMillis()

    println("Inter-cluster distance: " + obtainedModel.evaluateInterClusterDistances(Distances.Euclidean[Double, Double]))
    println("Intra-cluster distance: " + obtainedModel.evaluateIntraClusterDistances(Distances.Euclidean[Double, Double]))
    println("Training time: "+ (finalMoment - initialMoment))

    obtainedModel.render2D("PSO",0,1)
  }

}
