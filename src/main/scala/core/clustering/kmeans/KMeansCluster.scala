package core.clustering.kmeans

import core.DataSet
import core.clustering.model.CentroidCluster

/**
  * Created by Fer on 05/05/2016.
  */
class KMeansCluster(centroid: Array[Double], dataSet: DataSet) extends CentroidCluster(centroid, dataSet){

  def moveCenter: KMeansCluster = {
    require( members.nonEmpty, s"Cannot move the center of an empty cluster")

    // Creates the new cluster
    val newCluster = new KMeansCluster(stats.means,dataSet)

    // TODO: Instead of assgining all of them and then reassign them, why not simply assign them only once?
    // TODO: Some way to pass a copied version of the members list
    this.members.foreach(newCluster.+=(_))

    // Returns the new cluster.
    newCluster
  }
}
