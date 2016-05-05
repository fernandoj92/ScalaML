package core

/**
  * This class represents the Data structure that is going to be used to represent data in memory.
  * @param headers the column headers.
  * @param data  the matrix of values.
  */
case class DataSet(headers:List[String], data: Array[Array[Double]]) {

  def instanceSize: Int = data.head.length

}

// TODO: This class is not used at the moment because data rows are represented as matrix rows in the dataSet values.
class Observation(index: Int, values: Array[Double]){

}
