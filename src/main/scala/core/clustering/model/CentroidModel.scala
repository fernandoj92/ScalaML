package core.clustering.model

import core.DataSet
import core.plotting.Plotting
import core.util.Distances.DistanceFunc
import org.jfree.chart.{ChartFactory, ChartFrame}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

/**
  * This class represents
  */
class CentroidModel(clusters: List[CentroidCluster],
                    assignments :Array[Int],
                    dataSet: DataSet) extends Plotting{

  /**
    *
    * @return
    */
  def getClusters: List[CentroidCluster] = this.clusters

  /**
    *
    * @return
    */
  def getAssignments : Array[Int] = this.assignments

  def evaluateInterClusterDistances(distanceFunc: DistanceFunc): Double ={
    val visited = new Array[Boolean](clusters.size)
    var distanceSum = 0.0

    for(i <- clusters.indices)
      if(!visited(i))
        for(j <-clusters.indices)
          if(i!=j && !visited(j))
            distanceSum += distanceFunc(clusters(i).getCentroid, clusters(j).getCentroid)

    distanceSum/clusters.size
  }

  def evaluateIntraClusterDistances(distanceFunc: DistanceFunc): Double ={
    val distanceSum = clusters.map(x => x.distancesSum(distanceFunc)).sum
    distanceSum/dataSet.data.size
  }

  /**
    *
    * @param name
    * @param dimensionX
    * @param dimensionY
    */
  def render2D(name:String, dimensionX: Int, dimensionY: Int){

    // Por cada cluster creamos una serie XY
    val xyCollection = new XYSeriesCollection()
    var index = 0

    for(cluster <- clusters){
      val series = new XYSeries("Cluster_"+index)
      index = index +1

      // Iteramos por los miembros del cluster
      for(index <- cluster.getMembers){
        val x = dataSet.data(index)(dimensionX)
        val y = dataSet.data(index)(dimensionY)
        series.add(x,y)
      }
      xyCollection.addSeries(series)
    }

    // Ahora creamos una serie unicamente para los centroides
    val centroidSeries = new XYSeries("Centroids")
    for(cluster <- clusters){
      val cX = cluster.getCentroid(dimensionX)
      val cY = cluster.getCentroid(dimensionY)
      centroidSeries.add(cX, cY)
    }
    xyCollection.addSeries(centroidSeries)

    val chart = ChartFactory.createScatterPlot(name, "Dim_"+dimensionX, "Dim_"+dimensionY, xyCollection)

    // create and display a frame...
    val  frame = new ChartFrame("First", chart)
    frame.pack()
    frame.setVisible(true)
  }
}
