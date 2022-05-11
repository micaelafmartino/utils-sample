package dependencies

import scala.concurrent.duration.Duration
import scala.util.matching.Regex

/** A bunch of legacy First Foundry commons' validators (there were a lot of
  * them here, just left the few used in Validations)
  */
object Validator {

  /* ... */

  def nonBlankString[ErrorType](validateThis: String, error: ErrorType): ValidationNel[ErrorType, String] = {
    if ((validateThis == null) || validateThis.trim.isEmpty) error.failureNel
    else validateThis.successNel
  }

  /** https://github.com/ktoso/scala-words/blob/master/src/main/scala/pl/project13/scala/concurrent/util/Duration.scala#L109-L115
    */
  def duration[ErrorType](validateThis: String, error: ErrorType): ValidationNel[ErrorType, String] = {
    try {
      Duration.create(validateThis)
      validateThis.successNel
    } catch {
      case _: NumberFormatException => error.failureNel
    }
  }

  /** Succeeds when string is not null and matches the withRegex format. */
  def regex[ErrorType](validateThis: String, withRegex: Regex, error: ErrorType): ValidationNel[ErrorType, String] = {
    if ((validateThis == null) || !withRegex.pattern.matcher(validateThis).matches()) error.failureNel
    else validateThis.successNel
  }

  /* ... */

}
