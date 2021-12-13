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

  def routes(
    controllerName: String,
    path: String,
    httpVerb: String,
    method: String,
    params: String
  ): String =
    if (params.isEmpty)
      s"""
       |$httpVerb $path  $controllerName.$method()
       |""".stripMargin
    else
      s"""
       |$httpVerb $path  $controllerName.$method($params)
       |""".stripMargin
}
