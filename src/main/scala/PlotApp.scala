import org.jfree.chart.{ChartFrame, ChartFactory, JFreeChart}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection, XYDataset}

import scala.util.Random

/**
  * Created by Fernando on 4/12/2016.
  */
object PlotApp {

  def main(args: Array[String]): Unit = {

    val dataset = createXYDataSet
    val chart = createChart(dataset)

    // create and display a frame...
    val  frame = new ChartFrame("First", chart);
    frame.pack();
    frame.setVisible(true);

  }

  def createXYDataSet: XYDataset={
    val xyCollection = new XYSeriesCollection()
    val r = new Random()

    val series1 = new XYSeries("Random1")
    for ( index <- 1 to 100) {
      val x = r.nextDouble()
      val y = r.nextDouble()
      series1.add(x, y);
    }

    val series2 = new XYSeries("Random2") //the string is the key
    for ( index <- 1 to 100) {
      val x = r.nextDouble()
      val y = r.nextDouble()
      series2.add(x, y);
    }

    xyCollection.addSeries(series1)
    xyCollection.addSeries(series2)

    xyCollection
  }

  def createChart(xydataset: XYDataset): JFreeChart = {
    ChartFactory.createScatterPlot("Random", "X", "Y", xydataset)
  }

}
