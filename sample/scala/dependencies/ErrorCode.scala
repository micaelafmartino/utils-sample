package dependencies

/** Simplified project's error handling and codes model */
sealed abstract class ErrorCode(val code: Int, val description: String)

/** A bunch of ErrorCodes to be used in Validations and some utils */
object ErrorCode {

  /** JavaEnum-style Lookup by name */
  def valueOf(s: String): Option[ErrorCode] = ???

  case class ValidationError(errorsDescription: String) extends ErrorCode(42, s"Validation Error(s): $errorsDescription")

  case object ThingyNameInvalid extends ErrorCode(2, "")
  case object SymbolInvalid extends ErrorCode(3, "")
  case object AmountOfMoneyRange extends ErrorCode(5, "")
  case object FeeRange extends ErrorCode(6, "")
  case object IdInvalid extends ErrorCode(7, "")
  case object InvalidKey extends ErrorCode(8, "")
  case object InvalidLeftId extends ErrorCode(9, "")
  case object LeftIdMissing extends ErrorCode(10, "")
  case object MetadataMaxLength extends ErrorCode(11, "")
  case object InvalidRange extends ErrorCode(12, "")
  case object DurationStringInvalid extends ErrorCode(13, "")
  case object InvalidDuration extends ErrorCode(14, "")
  case object InvalidPrefixedId extends ErrorCode(15, "")
  case object InvalidSlope extends ErrorCode(16, "")
}

final case class CustomRootException(errorCode: Int, exceptionMessage: String, cause: Throwable) extends RuntimeException
