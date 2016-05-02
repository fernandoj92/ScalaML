import core.clustering.KMeans
import core.util.Distances
import core.{DataSource, DataSourceConfig}

object DemoApp{

  def main(args: Array[String]): Unit = {
    println("DemoApp esta en marcha")

    val url = System.getProperty("user.dir") + "\\data\\iris-modified.csv"
    val dataSource = DataSource(DataSourceConfig(url,normalize = true))
    //Devuelve un Try, asi que no valdria con eso, habria que comprobar que ha sido un exito
    val dataSet = dataSource.generateDataSet()

    println("Dataset generado")

    val kmeansAlgorithm = new KMeans(3, 10, Distances.Euclidean[Double, Double])

    println("KMeans algorithm created")

    // http://stackoverflow.com/questions/25593567/scala-throw-error-vs-return-try
    val obtainedModel = kmeansAlgorithm.train(dataSet)

    obtainedModel match {
      case Some(model) => model.render2D(0,1)
      case None => println("There was a problem when training the model")
    }



  }
}
