import core.clustering.hybrid.HybridPSOKMeans
import core.clustering.kmeans.KMeans
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
    // diabetesTest()
  }
  // Checked all, obtiene datos algo raros con instancias superpuestas
  // Este ejemplo tiene valores diferentes a los del PSO normal por lo que eso tmb influye en los resultados
  private def irisTest(): Unit ={
    val url = System.getProperty("user.dir") + "\\data\\iris-modified.csv"
    val dataSource = DataSource(DataSourceConfig(url, normalize = true))
    //Devuelve un Try, asi que no valdria con eso, habria que comprobar que ha sido un exito
    val dataSet = dataSource.generateDataSet()

    println("Dataset generado")

    // Crear instancias de algoritmo para mandarlo al hibrido
    val kmeansAlgorithm = new KMeans(3, 10, Distances.Euclidean[Double, Double])
    println("KMeans algorithm created")

    val particleConfig = new PSOParticleConfig(dataSet,Distances.Euclidean[Double, Double], 0.2, 0.7, 0.5 )
    val psoClusteringAlgorithm = new PSO(particleConfig, 3, 10, 3, Distances.Euclidean[Double, Double])
    println("PSO algorithm created")

    val hybridClusteringAlgorithm = new HybridPSOKMeans(kmeansAlgorithm, psoClusteringAlgorithm)
    println("Hybrid-PSO algorithm created")

    // http://stackoverflow.com/questions/25593567/scala-throw-error-vs-return-try
    val initialMoment = System.currentTimeMillis()
    val obtainedModel = hybridClusteringAlgorithm.train(dataSet)
    val finalMoment = System.currentTimeMillis()

    println("Inter-cluster distance: " + obtainedModel.evaluateInterClusterDistances(Distances.Euclidean[Double, Double]))
    println("Intra-cluster distance: " + obtainedModel.evaluateIntraClusterDistances(Distances.Euclidean[Double, Double]))
    println("Training time: "+ (finalMoment - initialMoment))

    val stop = false
    while(!stop){

      val ln = readLine("Dimension 1: ")
      val ln2 = readLine("Dimension 2: ")

      println("Dimensions ["+ ln.toInt + ","+ln2.toInt+"]")
      obtainedModel.render2D("Hybrid-PSO",ln.toInt,ln2.toInt)
    }
  }

  //Este ejemplo tiene valores diferentes a los del PSO normal por lo que eso tmb influye en los resultados
  private def diabetesTest(): Unit ={

    val url = System.getProperty("user.dir") + "\\data\\diabetes-modified.csv"
    val dataSource = DataSource(DataSourceConfig(url, normalize = true))
    //Devuelve un Try, asi que no valdria con eso, habria que comprobar que ha sido un exito
    val dataSet = dataSource.generateDataSet()

    println("Dataset generado")

    // Crear instancias de algoritmo para mandarlo al hibrido
    val kmeansAlgorithm = new KMeans(3, 10, Distances.Euclidean[Double, Double])
    println("KMeans algorithm created")

    val particleConfig = new PSOParticleConfig(dataSet,Distances.Euclidean[Double, Double], 0.2, 0.7, 0.5 )
    val psoClusteringAlgorithm = new PSO(particleConfig, 3, 10, 3, Distances.Euclidean[Double, Double])
    println("PSO algorithm created")

    val hybridClusteringAlgorithm = new HybridPSOKMeans(kmeansAlgorithm, psoClusteringAlgorithm)
    println("Hybrid-PSO algorithm created")

    // http://stackoverflow.com/questions/25593567/scala-throw-error-vs-return-try
    val initialMoment = System.currentTimeMillis()
    val obtainedModel = hybridClusteringAlgorithm.train(dataSet)
    val finalMoment = System.currentTimeMillis()

    println("Inter-cluster distance: " + obtainedModel.evaluateInterClusterDistances(Distances.Euclidean[Double, Double]))
    println("Intra-cluster distance: " + obtainedModel.evaluateIntraClusterDistances(Distances.Euclidean[Double, Double]))
    println("Training time: "+ (finalMoment - initialMoment))

    val stop = false
    while(!stop){

      val ln = readLine("Dimension 1: ")
      val ln2 = readLine("Dimension 2: ")

      println("Dimensions ["+ ln.toInt + ","+ln2.toInt+"]")
      obtainedModel.render2D("Hybrid-PSO",ln.toInt,ln2.toInt)
    }
  }

}
