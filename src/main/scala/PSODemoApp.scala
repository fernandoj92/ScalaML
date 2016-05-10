import core.clustering.pso.{PSOParticleConfig, PSO}
import core.util.Distances
import core.{DataSourceConfig, DataSource}

/**
  * Created by Fer on 10/05/2016.
  */
object PSODemoApp {

  def main(args: Array[String]): Unit = {
    println("DemoApp esta en marcha")

    val url = System.getProperty("user.dir") + "\\data\\iris-modified.csv"
    val dataSource = DataSource(DataSourceConfig(url, normalize = true))
    //Devuelve un Try, asi que no valdria con eso, habria que comprobar que ha sido un exito
    val dataSet = dataSource.generateDataSet()

    println("Dataset generado")

    val particleConfig = new PSOParticleConfig(dataSet,Distances.Euclidean[Double, Double] )
    val psoClusteringAlgorithm = new PSO(particleConfig, 3, 10, 3, Distances.Euclidean[Double, Double])

    println("PSO algorithm created")

    // http://stackoverflow.com/questions/25593567/scala-throw-error-vs-return-try
    val obtainedModel = psoClusteringAlgorithm.train(dataSet)

    obtainedModel match {
      case Some(model) => model.render2D("PSO",0,1)
      case None => println("There was a problem when training the model")
    }
  }

}
