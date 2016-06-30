import core.clustering.kmeans.KMeans
import core.util.Distances
import core.{DataSource, DataSourceConfig}

object KMeansDemoApp{

  def main(args: Array[String]): Unit = {
    println("DemoApp esta en marcha")

     //irisTest()
     diabetesTest()
  }

  // Checked [0,1] se ve mu bien, restarts randoms
  // Checked [1,2] se ve mu bien, restarts randoms
  private def irisTest(): Unit ={
    val url = System.getProperty("user.dir") + "\\data\\iris-modified.csv"
    val dataSource = DataSource(DataSourceConfig(url,normalize = true))
    //Devuelve un Try, asi que no valdria con eso, habria que comprobar que ha sido un exito
    val dataSet = dataSource.generateDataSet()

    println("Dataset generado")

    val kmeansAlgorithm = new KMeans(3, 10, Distances.Euclidean[Double, Double])

    println("KMeans algorithm created")

    // http://stackoverflow.com/questions/25593567/scala-throw-error-vs-return-try
    val initialMoment = System.currentTimeMillis()
    val obtainedModel = kmeansAlgorithm.train(dataSet)
    val finalMoment = System.currentTimeMillis()

    println("Inter-cluster distance: " + obtainedModel.evaluateInterClusterDistances(Distances.Euclidean[Double, Double]))
    println("Intra-cluster distance: " + obtainedModel.evaluateIntraClusterDistances(Distances.Euclidean[Double, Double]))
    println("Training time: "+ (finalMoment - initialMoment))


    val stop = false
    while(!stop){

      val ln = readLine("Dimension 1: ")
      val ln2 = readLine("Dimension 2: ")

      println("Dimensions ["+ ln.toInt + ","+ln2.toInt+"]")
      obtainedModel.render2D("KMeans",ln.toInt,ln2.toInt)
    }
  }
  // Checked [1,5]
  // Checked [0,3]
  private def diabetesTest(): Unit ={
    val url = System.getProperty("user.dir") + "\\data\\diabetes-modified.csv"
    val dataSource = DataSource(DataSourceConfig(url,normalize = true))
    //Devuelve un Try, asi que no valdria con eso, habria que comprobar que ha sido un exito
    val dataSet = dataSource.generateDataSet()

    println("Dataset generado")

    val kmeansAlgorithm = new KMeans(3, 10, Distances.Euclidean[Double, Double])

    println("KMeans algorithm created")

    // http://stackoverflow.com/questions/25593567/scala-throw-error-vs-return-try
    val initialMoment = System.currentTimeMillis()
    val obtainedModel = kmeansAlgorithm.train(dataSet)
    val finalMoment = System.currentTimeMillis()

    println("Inter-cluster distance: " + obtainedModel.evaluateInterClusterDistances(Distances.Euclidean[Double, Double]))
    println("Intra-cluster distance: " + obtainedModel.evaluateIntraClusterDistances(Distances.Euclidean[Double, Double]))
    println("Training time: "+ (finalMoment - initialMoment))

    val stop = false
    while(!stop){

      val ln = readLine("Dimension 1: ")
      val ln2 = readLine("Dimension 2: ")

      println("Dimensions ["+ ln.toInt + ","+ln2.toInt+"]")
      obtainedModel.render2D("KMeans",ln.toInt,ln2.toInt)
    }
  }
}
