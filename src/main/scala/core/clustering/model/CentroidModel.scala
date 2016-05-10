package core.clustering.model

import core.DataSet
import core.plotting.Plotting
import org.jfree.chart.{ChartFrame, ChartFactory}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

/**
  * Created by Fernando on 4/12/2016.
  */
class CentroidModel(clusters: List[CentroidCluster], dataSet: DataSet) extends Plotting{

  /**
    *
    * @return
    */
  def getClusters: List[CentroidCluster] = this.clusters

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
