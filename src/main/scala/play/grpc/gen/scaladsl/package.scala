package play.grpc.gen

package object scaladsl {

  abstract sealed class AbstractHttpVerb private[scaladsl] (val name: String)
  object HttpVerb {
    case object GET   extends AbstractHttpVerb("GET")
    case object POST  extends AbstractHttpVerb("POST")
    case object PATCH extends AbstractHttpVerb("PATCH")
    case object PUT   extends AbstractHttpVerb("PUT")
  }

  final case class GrpcMethodInfo(
    name: String,
    inputType: String,
    inputProto: String,
    outputProto: String,
    private val verb: AbstractHttpVerb
  ) {
    val verbName: String = verb.name
  }

  // Mimics com.google.api.HttpRule from https://github.com/googleapis/googleapis/blob/master/google/api/http.proto
  sealed trait GoogleHttpRule

  object GoogleHttpRule {
    abstract sealed class AbstractHttpRule private[scaladsl] (val index: Int) extends GoogleHttpRule

    case object SELECTOR_FIELD_NUMBER extends AbstractHttpRule(1)

    case object GET_FIELD_NUMBER extends AbstractHttpRule(2)

    case object PUT_FIELD_NUMBER extends AbstractHttpRule(3)

    case object POST_FIELD_NUMBER extends AbstractHttpRule(4)

    case object DELETE_FIELD_NUMBER extends AbstractHttpRule(5)

    case object PATCH_FIELD_NUMBER extends AbstractHttpRule(6)

    case object BODY_FIELD_NUMBER extends AbstractHttpRule(7)

    case object CUSTOM_FIELD_NUMBER extends AbstractHttpRule(8)

    case object ADDITIONAL_BINDINGS_FIELD_NUMBER extends AbstractHttpRule(11)

    case object RESPONSE_BODY_FIELD_NUMBER extends AbstractHttpRule(12)

    // Smart c-tor that guards construction of an invalid httpRule
    def fromIndex(ind: Int): Option[GoogleHttpRule] =
      ind match {
        case 1  => Some(SELECTOR_FIELD_NUMBER)
        case 2  => Some(GET_FIELD_NUMBER)
        case 3  => Some(PUT_FIELD_NUMBER)
        case 4  => Some(POST_FIELD_NUMBER)
        case 5  => Some(DELETE_FIELD_NUMBER)
        case 6  => Some(PATCH_FIELD_NUMBER)
        case 7  => Some(BODY_FIELD_NUMBER)
        case 8  => Some(CUSTOM_FIELD_NUMBER)
        case 11 => Some(ADDITIONAL_BINDINGS_FIELD_NUMBER)
        case 12 => Some(RESPONSE_BODY_FIELD_NUMBER)
        case _  => None
      }
  }

}
