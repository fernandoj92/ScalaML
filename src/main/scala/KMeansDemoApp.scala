import core.clustering.kmeans.KMeans
import core.util.Distances
import core.{DataSource, DataSourceConfig}

object KMeansDemoApp{

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

    obtainedModel.render2D("KMeans",0,1)
  }
}
