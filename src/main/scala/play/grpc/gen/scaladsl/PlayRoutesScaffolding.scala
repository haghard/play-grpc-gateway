package play.grpc.gen.scaladsl

object PlayRoutesScaffolding {

  def routesHeader(): String =
    s"""
       |# Routes
       |# This file defines all application routes (Higher priority routes first)
       |
       |""".stripMargin

  def routesFooter(cntPkgName: String): String =
    s"""
       |
       |# Map static resources from the /public folder to the /assets URL path
       |GET     /assets/*file   $cntPkgName.Assets.versioned(path="/public", file: Asset)
       |
       |""".stripMargin

  def routesPostRoute(cntPkgName: String, controllerName: String, path: String, method: String): String =
    s"""
       |POST  $path  $cntPkgName.$controllerName.$method()
       |
       |""".stripMargin

  def routesGetRoute(
    controllerName: String,
    getPath: String,
    method: String,
    params: String
  ): String =
    if (params.isEmpty)
      s"""
         |GET $getPath  $controllerName.$method()
         |""".stripMargin
    else
      s"""
         |GET $getPath  $controllerName.$method($params)
         |""".stripMargin

}
