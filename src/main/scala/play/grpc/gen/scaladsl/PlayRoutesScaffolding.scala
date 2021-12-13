package play.grpc.gen.scaladsl

object PlayRoutesScaffolding {

  def routesHeader(): String =
    s"""
       |# Routes
       |# This file defines all application routes (Higher priority routes first)
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
