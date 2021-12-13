package play.grpc.gen.scaladsl

import akka.grpc.gen.Logger
import akka.grpc.gen.scaladsl.{ScalaCodeGenerator, Service}
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import templates.PlayScala.txt.{Context, HttpController}

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import scala.collection.immutable
import scala.jdk.CollectionConverters.asScalaBufferConverter
import scala.util.Using

class PlayScalaHttpServiceCodeGenerator extends ScalaCodeGenerator {

  override def name: String = "play-grpc-service-scala"

  override def perServiceContent = super.perServiceContent + generateHttpService

  import PlayScalaHttpServiceCodeGenerator._

  val generateHttpService: (Logger, Service) => scala.collection.immutable.Seq[CodeGeneratorResponse.File] =
    (logger, service) => {
      new File(s"./app/$CntrPkgName").mkdir()
      var methodsWithMetaInfo: Vector[GrpcMethodInfo] = Vector.empty

      val b                  = CodeGeneratorResponse.File.newBuilder()
      val controllerFileName = s"${service.name}Controller"

      val contextFileName = s"${service.name}Context"
      val contextFile     = new File(s"./app/$CntrPkgName/$contextFileName.scala")

      logger.info(s"★ ★ ★ Found ${service.name} in ${service.packageName} ★ ★ ★")

      val routesBuffer = new StringBuilder()
      routesBuffer.append(PlayRoutesScaffolding.routesHeader())

      service.methods.foreach { method =>
        val httpRuleStrBlock = method.options.toString.replace(s"$HttpRuleHttp:", "").split("\n")
        (1 to httpRuleStrBlock.size - 2).foreach { i =>
          httpRuleStrBlock(i).trim match {
            case HttpOptionExp(ind, value) =>
              val path = value.replaceAll("\"", "").trim
              GoogleHttpRule.fromIndex(ind.trim.toInt) match {
                case Some(validHttpRule) =>
                  validHttpRule match {
                    case GoogleHttpRule.GET_FIELD_NUMBER =>
                      logger.info(s"Found GET $path")
                      // For example /v1/messages/:name/age/:age - PathParams: [name,age]
                      val (playRouteUrlPath, pathParams) =
                        path match {
                          // GET wi /v1/{name=messages/*}
                          case PathParamKVExp(segments, k, v) =>
                            val param       = k.trim
                            val pathSegment = v.trim.replace("/*", "")
                            val getPath     = s"$segments$pathSegment/:$param"
                            (getPath, Set(param))
                          case _ => extractUrlPathParams(path)
                        }

                      val methodInputParamsWithTypes = extactInputParamsWithTypes(method)
                      val pathParametersWithTyped =
                        extractPathParametersWithTypes(pathParams, methodInputParamsWithTypes, method)

                      // Any fields in the request message which are not bound by the path template automatically become HTTP query parameters
                      // if there is no HTTP request body !!!!
                      val httpQueryParameters = methodInputParamsWithTypes.keySet.diff(pathParametersWithTyped.keySet)
                      val httpQueryParametersWithTypes =
                        httpQueryParameters.map(p => p -> methodInputParamsWithTypes(p)).toMap

                      val inputParamsStr = {
                        if (pathParametersWithTyped.nonEmpty && httpQueryParametersWithTypes.nonEmpty)
                          pathParametersWithTyped.map { case (p, t) => s"$p: $t" }.mkString(", ") + ", " +
                            httpQueryParametersWithTypes.map { case (p, t) => s"$p: $t" }.mkString(", ")
                        else if (pathParametersWithTyped.nonEmpty)
                          pathParametersWithTyped.map { case (p, t) => s"$p: $t" }.mkString(", ")
                        else httpQueryParametersWithTypes.map { case (p, t) => s"$p: $t" }.mkString(", ")
                      }

                      routesBuffer.append(
                        PlayRoutesScaffolding.routes(
                          s"${service.grpcName}$Postfix",
                          playRouteUrlPath,
                          HttpVerb.GET.name,
                          method.grpcName,
                          inputParamsStr
                        )
                      )

                      methodsWithMetaInfo = methodsWithMetaInfo :+ GrpcMethodInfo(
                        method.grpcName,
                        inputParamsStr,
                        method.inputType.getFullName,
                        method.outputType.getFullName,
                        HttpVerb.GET
                      )

                    // Turn POST, PATCH, PUT into POST ???
                    case GoogleHttpRule.POST_FIELD_NUMBER | GoogleHttpRule.PATCH_FIELD_NUMBER |
                        GoogleHttpRule.PUT_FIELD_NUMBER =>
                      val (playRouteUrlPath, pathParams) = extractUrlPathParams(path)
                      val methodInputParamsWithTypes     = extactInputParamsWithTypes(method)
                      val pathParametersWithTyped =
                        extractPathParametersWithTypes(pathParams, methodInputParamsWithTypes, method)

                      val inputParamsStr =
                        if (pathParametersWithTyped.isEmpty) ""
                        else pathParametersWithTyped.map { case (p, t) => s"$p: $t" }.mkString(", ")

                      routesBuffer.append(
                        PlayRoutesScaffolding.routes(
                          s"${service.grpcName}$Postfix",
                          playRouteUrlPath,
                          HttpVerb.POST.name,
                          method.grpcName,
                          inputParamsStr
                        )
                      )

                      methodsWithMetaInfo = methodsWithMetaInfo :+ GrpcMethodInfo(
                        method.grpcName,
                        inputParamsStr,
                        method.inputType.getFullName,
                        method.outputType.getFullName,
                        HttpVerb.POST
                      )

                    case GoogleHttpRule.SELECTOR_FIELD_NUMBER =>
                      throw new Exception(s"Not supported ${GoogleHttpRule.SELECTOR_FIELD_NUMBER}")
                    case GoogleHttpRule.DELETE_FIELD_NUMBER =>
                      throw new Exception(s"Not supported ${GoogleHttpRule.DELETE_FIELD_NUMBER}")
                    case GoogleHttpRule.CUSTOM_FIELD_NUMBER =>
                      throw new Exception(s"Not supported ${GoogleHttpRule.CUSTOM_FIELD_NUMBER}")
                    case GoogleHttpRule.BODY_FIELD_NUMBER =>
                    case GoogleHttpRule.RESPONSE_BODY_FIELD_NUMBER =>
                      throw new Exception(s"Not supported ${GoogleHttpRule.RESPONSE_BODY_FIELD_NUMBER}")
                    case GoogleHttpRule.ADDITIONAL_BINDINGS_FIELD_NUMBER =>
                      throw new Exception(s"Not supported ${GoogleHttpRule.ADDITIONAL_BINDINGS_FIELD_NUMBER}")
                  }
                case None =>
                  throw new Exception(
                    "Smth has changed in com.google.api.HttpRule from https://github.com/googleapis/googleapis/blob/master/google/api/http.proto"
                  )
              }
            case _ => throw new Exception("Failed to parse HttpRule")
          }
        }
        logger.info(s"rpc ${method.grpcName}(${method.inputType.getName}) returns (${method.outputType.getName})")
      }

      routesBuffer.append(PlayRoutesScaffolding.routesFooter(CntrPkgName))
      println(ANSI_RED_BACKGROUND + routesBuffer.toString() + ANSI_RESET)

      // 1. Routes file
      Using.resource(new FileOutputStream(new File(s"./conf/routes_${service.name}_gen")))(
        _.write(routesBuffer.toString().getBytes(StandardCharsets.UTF_8))
      )

      // 2. Context file.
      if (!contextFile.exists())
        Using.resource(new FileOutputStream(contextFile))(
          _.write(Context(service.name, CntrPkgName, methodsWithMetaInfo).body.getBytes(StandardCharsets.UTF_8))
        )

      // 3. Controller file.
      b.setContent(HttpController(service, methodsWithMetaInfo, CntrPkgName + "." + contextFileName).body)
      b.setName(s"${service.packageDir}/$controllerFileName.scala")

      logger.info(s"★ ★ ★ Generating ${service.packageName}.$controllerFileName  ★ ★ ★")
      immutable.Seq(b.build)
    }
}

object PlayScalaHttpServiceCodeGenerator extends PlayScalaHttpServiceCodeGenerator {
  val CntrPkgName = "controllers"

  val Postfix             = "Controller"
  val ANSI_RED_BACKGROUND = "\u001B[41m"
  val ANSI_RESET          = "\u001B[0m"

  val HttpOptionExp = """(\d*):(.*)""".r

  // google.protobuf.MethodOptions { HttpRule http = 72295728; }
  val HttpRuleHttp = 72295728

  // Support only primitives (non-message) type
  val supportedTypes = Set("STRING", "INT", "LONG", "BOOLEAN", "DOUBLE")

  // get: /v1/{name=messages/*}
  val PathParamKVExp = """(.+)\{(.+)=(.+)\}""".r

  /** (e.g. /v1/messages/{name}, /v1/messages/{name}/age/{age}
    */
  val PathParamsExp = """\{(.*?)}""".r

  // validate a URL path with no query params (e.g. /helloworld, /hello/world )
  val PathWithNoQueryParams = """^\/[/.a-zA-Z0-9-]+$""".r

  // turn INT -> Int
  def asPlayType(protoType: String): String = {
    val local = protoType.toLowerCase
    local.charAt(0).toUpper.toString + local.substring(1)
  }

  def extractUrlPathParams(path: String): (String, Set[String]) =
    if (PathWithNoQueryParams.findAllMatchIn(path).hasNext) {
      (path, Set.empty[String])
    } else {
      // POST with path params (e.g. /v1/messages/{name}, /v1/messages/{name}/age/{age}
      val matcher    = PathParamsExp.pattern.matcher(path)
      var pathParams = Set.empty[String]
      if (matcher.find()) {
        val pathParam =
          path.substring(matcher.start(), matcher.end()).trim.replaceAll("[\\{\\}]", "")
        pathParams = pathParams + pathParam
        while (matcher.find()) {
          val param =
            path.substring(matcher.start(), matcher.end()).trim.replaceAll("[\\{\\}]", "")
          pathParams = pathParams + param
        }
        val a = path.replace("{", ":").replace("}", "")
        val b = if (a.charAt(a.length - 1) == '/') a.substring(0, a.length - 1) else a
        (b, pathParams)
      } else throw new Exception(s"Smth's wrong with url $path !")
    }

  def extactInputParamsWithTypes(method: akka.grpc.gen.scaladsl.Method): Map[String, String] =
    method.inputType.getFields.asScala.map { fd =>
      val pType = fd.getJavaType.name()
      if (supportedTypes.contains(pType)) (fd.getName, asPlayType(pType))
      else
        throw new Exception(
          s"Found unsupported type $pType in ${method.inputType.getFullName}.${fd.getName}"
        )
    }.toMap

  def extractPathParametersWithTypes(
    pathParams: Set[String],
    methodInputParamsWithTypes: Map[String, String],
    method: akka.grpc.gen.scaladsl.Method
  ): Map[String, String] =
    pathParams.foldLeft(Map.empty[String, String]) { (acc, param) =>
      methodInputParamsWithTypes.get(param) match {
        case Some(methodType) =>
          acc + (param -> methodType)
        case None =>
          throw new Exception(
            s"Couldn't find method ($param) on ${method.inputType.getFullName}. Check your proto schema!"
          )
      }
    }
}
