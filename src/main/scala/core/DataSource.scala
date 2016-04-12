package core

import scala.util.Try

/**
  * This class represents the configuration of he DataSource class.
  * @param filePath the file path of the file that is going to be imported for analysis.
  * @param normalize if data needs to be normalized or not.
  * @param headerLines represent the number of header lines that data files will have. The default value is 1.
  */
case class DataSourceConfig(
                             filePath: String,
                             normalize: Boolean,
                             headerLines: Int = 1)

/**
  * This class takes the responsibility of reading and transforming data files into data structures used in the project.
  * @param config the configuration object representing its parameters.
  */
  // TODO: It only accepts Double values.
case class DataSource(config: DataSourceConfig){

  /** This method will be automatically called when trying to create an instance of this class.
    * It checks if the configuration is valid.
    */
  check(config)

  /**
    * Generates a DataSet from the data file if there are no exceptions.
    * @return the new dataSet object.
    */
  // TODO: check if it works with lower number of headers that it should have.
  def generateDataSet(): Try[DataSet] = Try{

    // Reads the file.
    val bufferedSource = io.Source.fromFile(config.filePath)

    // Stores the file's headers.
    val multipleHeaders =
      for(line <- 1 to config.headerLines)
        yield bufferedSource.getLines().next().split(",").map(_.trim()).toList
    val headers = multipleHeaders.reduceLeft(_ ++ _)

    // Stores the file's instance values as a matrix of Doubles.
    val instances =
      (for (line <- bufferedSource.getLines().drop(config.headerLines))
        yield line.split(",").map(column => column.trim().toDouble)).toArray

    bufferedSource.close()

    // If true, normalizes the dataSet's instances.
    if(config.normalize)
      DataSet(headers, normalize(instances))
    else
      DataSet(headers,instances)
  }

  /**
    * Normalizes the DataSet between [0,1].
    * @param data the matrix of double values.
    * @return the normalized matrix of double values.
    */
  // https://github.com/prnicolas/ScalaMl/blob/master/src/main/scala/org/scalaml/stats/MinMax.scala
  private def normalize(data: Array[Array[Double]]): Array[Array[Double]]={
    val minimum = new Array[Double](data.head.length)
    val maximum = new Array[Double](data.head.length)

    for(j <- data.head.indices){
      minimum(j) = data.head(j)
      maximum(j) = data.head(j)
      for(i <- data.indices){
        if(data(i)(j) < minimum(j))
          minimum(j) = data(i)(j)
        else if(data(i)(j) > maximum(j))
          maximum(j) = data(i)(j)
      }
    }

    val diff = for(i <- minimum.indices)  yield maximum(i)- minimum(i)
    val normalizedData = Array.ofDim[Double](data.length,data.head.length)

    for(j <- data.head.indices)
      for (i <- data.indices) {
        normalizedData(i)(j) = (data(i)(j) - minimum(j)) / diff(j)
      }

    normalizedData
  }

  /**
    * Checks if the configuration is valid.
    * @param config the configuration of the DataSource object.
    */
  // TODO: It could use a way of assuring that the X first lines are really header lines.
  private def check(config: DataSourceConfig): Unit =  {
    require( !config.filePath.isEmpty,
      "DataSource.check Undefined path for data source")
    require( config.headerLines <=0,
      s"DataSource.check Incorrect number of header lines ${config.headerLines} for data source")
  }

}


