import core.{DataSource, DataSourceConfig}

object DemoApp{

  def main(args: Array[String]): Unit = {
    println("DemoApp esta en marcha")

    val url = System.getProperty("user.dir") + "\\data\\iris-modified.csv"
    val dataSource = DataSource(DataSourceConfig(url,normalize = true))
    //Devuelve un Try, asi que no valdria con eso, habria que comprobar que ha sido un exito
    val dataSet = dataSource.generateDataSet()

    println("A ver que tal")


  }
}
