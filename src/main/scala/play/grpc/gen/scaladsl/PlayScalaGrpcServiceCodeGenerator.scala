package play.grpc.gen.scaladsl

import scala.collection.immutable
import akka.grpc.gen.Logger
import akka.grpc.gen.scaladsl.{ScalaCodeGenerator, Service}
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import templates.PlayScala.txt.PlayGrpc2HttpService

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.asScalaBufferConverter
import scala.util.Using

class PlayScalaHttpServiceCodeGenerator extends ScalaCodeGenerator {

  override def name: String = "play-grpc-service-scala"

  override def perServiceContent = super.perServiceContent + generateHttpService

  import PlayScalaHttpServiceCodeGenerator._

  val generateHttpService: (Logger, Service) => scala.collection.immutable.Seq[CodeGeneratorResponse.File] =
    (logger, service) => {
      var methodsWithParamTyped: Map[String, String] = Map.empty
      val b                                          = CodeGeneratorResponse.File.newBuilder()
      val outputFileName                             = s"${service.name}Controller"

      logger.info(s"★ ★ ★ Found ${service.name} in ${service.packageName} ${service.descriptor} ★ ★ ★")

      val routesBuffer = new StringBuilder()
      routesBuffer.append(PlayRoutesScaffolding.routesHeader())

      service.methods.foreach { method =>
        val httpRuleStrBlock = method.options.toString.replace(s"$HttpRuleHttp:", "").split("\n")
        (1 to httpRuleStrBlock.size - 2).foreach { i =>
          httpRuleStrBlock(i).trim match {
            case HttpOptionExp(ind, value) =>
              val path = value.replaceAll("\"", "").trim
              // TODO: handle toInt convertion errors
              GoogleHttpRule.fromIndex(ind.trim.toInt) match {
                case Some(validHttpRule) =>
                  validHttpRule match {
                    case GoogleHttpRule.GET_FIELD_NUMBER =>
                      println(s"Found GET $path")
                      val (pathWithParam, params) =
                        path match {
                          case PathParamKVExp(segments, k, v) =>
                            val paramName   = k.trim
                            val pathSegment = v.trim.replace("/*", "")
                            val getPath     = s"$segments$pathSegment/:$paramName"
                            (getPath, Set(paramName))
                          case _ =>
                            if (PathWithNoQueryParams.findAllMatchIn(path).hasNext) {
                              (path, Set.empty[String])
                            } else {
                              val matcher = PathParamsExp.pattern.matcher(path)
                              var params  = Set.empty[String]
                              if (matcher.find()) {
                                val param =
                                  path.substring(matcher.start(), matcher.end()).trim.replaceAll("[\\{\\}]", "")
                                params = params + param
                                while (matcher.find()) {
                                  val param =
                                    path.substring(matcher.start(), matcher.end()).trim.replaceAll("[\\{\\}]", "")
                                  params = params + param
                                }
                                val a = path.replace("{", ":").replace("}", "")
                                val b = if (a.charAt(a.length - 1) == '/') a.substring(0, a.length - 1) else a
                                (b, params)
                              } else throw new Exception(s"Smth's wrong with url $path !")
                            }
                          case _ => throw new Exception(s"Boom !!!")
                        }

                      val methodInputParamsWithTypes =
                        method.inputType.getFields.asScala.map { fd =>
                          val pType = fd.getJavaType.name()
                          if (supportedTypes.contains(pType)) (fd.getName, asPlayType(pType))
                          else
                            throw new Exception(
                              s"Found unsupported type $pType in ${method.inputType.getFullName}.${fd.getName}"
                            )
                        }.toMap

                      val pathParametersWithTyped = params.foldLeft(Map.empty[String, String]) { (acc, param) =>
                        methodInputParamsWithTypes.get(param) match {
                          case Some(methodType) =>
                            acc + (param -> methodType)
                          case None =>
                            throw new Exception(
                              s"Couldn't find method ($param) on ${method.inputType.getFullName}. Check your proto schema!"
                            )
                        }
                      }

                      // Any fields in the request message which are not bound by the path template automatically become HTTP query parameters
                      // if there is no HTTP request body !!!!
                      val queryParameters = methodInputParamsWithTypes.keySet.diff(pathParametersWithTyped.keySet)
                      val queryParametersWithTypes = queryParameters.map(p => p -> methodInputParamsWithTypes(p)).toMap

                      // turn it into a string.
                      val paramsStr = pathParametersWithTyped.map { case (p, t) => s"$p: $t" }.mkString(", ") + ", " +
                        queryParametersWithTypes.map { case (p, t) => s"$p: $t" }.mkString(", ")

                      routesBuffer.append(
                        PlayRoutesScaffolding.routesGetRoute(
                          s"${service.grpcName}$Postfix",
                          pathWithParam,
                          method.grpcName,
                          paramsStr
                        )
                      )

                      methodsWithParamTyped = methodsWithParamTyped + (method.grpcName -> paramsStr)

                    case GoogleHttpRule.PUT_FIELD_NUMBER =>
                      ???
                    case GoogleHttpRule.POST_FIELD_NUMBER =>
                      ???
                    case GoogleHttpRule.SELECTOR_FIELD_NUMBER =>
                      throw new Exception(s"Not supported ${GoogleHttpRule.SELECTOR_FIELD_NUMBER}")
                    case GoogleHttpRule.DELETE_FIELD_NUMBER =>
                      throw new Exception(s"Not supported ${GoogleHttpRule.DELETE_FIELD_NUMBER}")
                    case GoogleHttpRule.PATCH_FIELD_NUMBER =>
                      throw new Exception(s"Not supported ${GoogleHttpRule.PATCH_FIELD_NUMBER}")
                    case GoogleHttpRule.CUSTOM_FIELD_NUMBER =>
                      throw new Exception(s"Not supported ${GoogleHttpRule.CUSTOM_FIELD_NUMBER}")
                    case GoogleHttpRule.BODY_FIELD_NUMBER =>
                      throw new Exception(s"Not supported ${GoogleHttpRule.BODY_FIELD_NUMBER}")
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

      routesBuffer.append(PlayRoutesScaffolding.routesFooter("controllers")) // service.packageName
      println(ANSI_RED_BACKGROUND + routesBuffer.toString() + ANSI_RESET)

      val routesFile = new File(s"./conf/routes_${service.name}")
      Using.resource(new FileOutputStream(routesFile))(
        _.write(routesBuffer.toString().getBytes(StandardCharsets.UTF_8))
      )

      b.setContent(PlayGrpc2HttpService(service, methodsWithParamTyped).body)
      b.setName(s"${service.packageDir}/$outputFileName.scala")

      logger.info(s"★ ★ ★ Generating ${service.packageName}.$outputFileName  ★ ★ ★")
      immutable.Seq(b.build)
    }
}

object PlayScalaHttpServiceCodeGenerator extends PlayScalaHttpServiceCodeGenerator {
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
}
