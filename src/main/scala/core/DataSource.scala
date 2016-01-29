package core

import scala.util.Try

case class DataSourceConfig(
                             filePath: String,
                             normalize: Boolean,
                             headerLines: Int = 1)

case class DataSource(config: DataSourceConfig){
  check(config)

  def generateDataSet(): Try[DataSet] = Try{
    val bufferedSource = io.Source.fromFile(config.filePath)

    val multipleHeaders =
      for(line <- 1 to config.headerLines)
        yield bufferedSource.getLines().next().split(",").map(_.trim()).toList
    val headers = multipleHeaders.reduceLeft(_ ++ _)

    val instances =
      (for (line <- bufferedSource.getLines().drop(config.headerLines))
        yield line.split(",").map(column => column.trim().toDouble)).toArray

    bufferedSource.close()

    if(config.normalize)
      DataSet(headers, normalize(instances))
    else
      DataSet(headers,instances)
  }

  // https://github.com/prnicolas/ScalaMl/blob/master/src/main/scala/org/scalaml/stats/MinMax.scala
  //Normalize(0,1) by default
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

  private def check(config: DataSourceConfig): Unit =  {
    require( !config.filePath.isEmpty,
      "DataSource.check Undefined path for data source")
    require( config.headerLines >=0,
      s"DataSource.check Incorrect number of header lines ${config.headerLines} for data source")
  }

}


