package samples.utils.model

import dependencies.ErrorCode._
import dependencies.RegexConstants.{AlphaNumericRegex, AlphanumericWithDashAndUnderscores, ValidNameRegex}
import dependencies.Validator.nonBlankString
import dependencies.{ErrorCode, Validator => LegacyValidator}
import samples.utils.model.Validations.CommonValidators
import samples.utils.model.Validator.{NoError, Success, Validation, ValidatorF}

import com.fasterxml.jackson.annotation.JsonIgnore
import play.api.libs.json.JsValue
import scalaz.Scalaz.ToValidationOps
import scalaz.{Failure, NonEmptyList, ValidationNel}

import java.math.MathContext
import scala.collection.compat.immutable.LazyList
import scala.concurrent.duration.Duration
import scala.language.implicitConversions
import scala.util.Try
import scala.util.matching.Regex

/** Provides some wrappers to legacy Validation API (which misuses Scalaz's
  * Validations), common [[Validator]]s and utils to test them
  */
trait Validations {

  /** Collects all the failed validations and throws a `ValidationError` with
    * the summary, if any.
    */
  protected[this] def validate(validations: Validation*): Unit = validateAndThen(validations: _*)(errorCodes => throw ValidationError(nelToString(errorCodes)))

  /** Collects all the failed validations and pass them to the given
    * errorsHandler. Useful for logging instead of throwing, for example.
    */
  protected[this] def validateAndThen(validations: Validation*)(errorsHandler: NonEmptyList[ErrorCode] => Unit): Unit = {
    validations
      .collect { case scalaz.Failure(e) => e }
      .reduceOption(_ append _)
      .fold(())(errorsHandler)
  }

  /** Root of predefined common [[Validator]]s */
  @JsonIgnore protected[this] val Valid: CommonValidators = Validations.Valid
}

object Validations {
  val Valid: CommonValidators = new CommonValidators {}

  /** Predefined common [[Validator]] s with specific domain semantics.
    *
    * Except a few special cases (like `Name`), we should put here only
    * 'complete' Validators, including the condition to verify and the
    * errorCode, only expecting the value to validate.
    *
    * They should 'sound' ok as an english sentence (like ScalaTest's DSL):
    *
    *   - Valid.PrimaryKey -> ok
    *   - Valid.RequestId -> ok
    *   - Valid.Name(specificErrorCode) -> this is a grey one because you have
    *     to pass the specific error, but still it sounds ok saying `Valid.name`
    *   - Valid.Regex(theRegex)(theErrorCode) -> nope, it sounds weird, you are
    *     not checking the regex is valid, you want to check that some string
    *     matches the regex pattern. In this case you can do something like:
    *
    * {{{
    *   object Validator {
    *     ...
    *     def regex(implicit e: ErrorCode): Validator[String] = ???
    *     ...
    *   }
    *
    *   object Valid {
    *     ...
    *     val UserName = Validator.regex(theUserNameRegex)(SomeCodeForInvalidUserName)
    *     ...
    *   }
    * }}}
    *
    * and use it like the rest: Valid.UserName (sounds great!)
    */
  trait CommonValidators {
    /* ***************************************************************************
     *
     * NOTE: Add unit tests for any new validator you put in here.
     *
     * ************************************************************************* */

    /** Pass when the validated thingy's name matches name regex pattern */
    lazy val SpecificThingyName: Validator[String] = Name(ThingyNameInvalid)

    /** Pass when the validated symbol is:
      *   - not blank
      *   - only composed of alpha-numeric chars
      *   - 10 chars long max
      */
    lazy val SomeSymbol: Validator[String] = {
      implicit val e: ErrorCode = SymbolInvalid
      Validator.nonBlank && Validator.matches(AlphaNumericRegex) && Validator.max(10)(e).contramap(_.length)
    }

    /** Pass when the validated coin amount is greater than `min` */
    def MoneyAmount(min: Int): Validator[BigInt] = Validator.min(BigInt(min))(AmountOfMoneyRange)

    /** Fail with the given error if the validated condition is not met. */
    def Condition(error: ErrorCode): Validator[Boolean] = Validator.onCondition[Boolean](identity)(error)

    /** Pass when the validated fee is between `min` and `max` (inclusive) */
    def Fee(min: BigInt = 0, max: BigInt): Validator[BigInt] = Validator.range(min, max)(FeeRange)

    /** Pass when the validated value is nonBlank */
    lazy val SomeSpecificId: Validator[String] = Validator.nonBlank(IdInvalid)

    /** Pass when the validated string:
      *   - is not blank
      *   - is no longer than 128 chars
      *   - only has Alphanumeric plus '-' and '_' chars
      */
    lazy val Key: Validator[String] = {
      implicit val e: ErrorCode = InvalidKey
      Validator.nonBlank && Validator.max(128)(e).contramap(_.length) && Validator.matches(AlphanumericWithDashAndUnderscores)
    }

    /** Pass unless the LEFT_ID is present but blank OR is not present but the
      * the RIGHT_ID is.
      *
      * (LEFT_ID, RIGHT_ID):
      *   - (Some(" "), _) => fail
      *   - (None, Some(_)) => fail
      *   - (None, None) => pass
      *   - (Some("non-blank-id"), None) => pass
      *   - (Some("non-blank-id"), Some("id")) => pass
      */
    lazy val IdPair: Validator[(Option[String], Option[String])] =
      Validator
        .nonBlank(InvalidLeftId) // if id is present, it must not be blank
        .opt
        .contramap[(Option[String], Option[String])] { case (id, _) => id } &&
        Validator
          .onCondition[(Option[String], Option[String])] {
            case (None, Some(_)) => false // if id is absent, fpid must be absent too
            case _               => true
          }(LeftIdMissing)

    /** Pass when the validated json's size does not exceed `maxLength` */
    def Metadata(maxLength: Int): Validator[JsValue] = Validator.max(maxLength)(MetadataMaxLength).contramap((json: JsValue) => json.toString().length)

    /** Pass when the validated string matches name regex pattern */
    def Name(implicit e: ErrorCode): Validator[String] = Validator.nonBlank && Validator.matches(ValidNameRegex)

    def SomeRange(currentTime: Long): Validator[Long] = Validator.range(1L, currentTime)(InvalidRange)

    /** Pass when the validated string represents a valid [[Duration]] value AND
      * the duration is not negative
      */
    lazy val Duration: Validator[String] =
      Validator.duration()(DurationStringInvalid) &&
        Validator.onCondition[String] {
          case d if Duration(d).toSeconds >= 0 => true
          case _                               => false
        }(InvalidDuration)

    lazy val PrefixedId: Validator[String] = Validator.onCondition[String](id => Seq("prefix1:", "prefix2:", "prefix3:").exists(id.startsWith))(InvalidPrefixedId)

    /** Pass when the validated slope value:
      *   - is greater than zero AND
      *   - has 19 or less figures to the right of the decimal point (only first
      *     8 significant figures are considered)
      */
    lazy val Slope: Validator[BigDecimal] = {
      implicit val e: ErrorCode = InvalidSlope
      Validator.min(BigDecimal(0), inclusive = false)(e) ++ Validator.decimalPrecision(8, 19)
    }

    /* ***************************************************************************
     *
     * NOTE: Add unit tests for any new validator you put in here.
     *
     * ************************************************************************* */
  }
}

/** Base class for Validators.
  *
  * A [[Validator]] is a function that, given a value of type `T`, returns a
  * [[Validation]] [T] which contains either the value if it passed the defined
  * validations or a [[ErrorCode]] describing the error.
  */
trait Validator[T] extends ValidatorF[T] {

  /** Represents the validation per se. Given a value to validate, returns a
    * list with the reasons of why the value didn't pass the validations or an
    * empty list if the value passed.
    */
  def verify(value: T): Seq[ErrorCode]

  /** Validate a given value. If it passes the validations, this is a NOOP.
    * Otherwise throws a [[ValidationError]] with a summary of the failures.
    */
  def validate(value: T): Unit = {
    verify(value) match {
      case h :: t => throw ValidationError(nelToString(NonEmptyList(h, t: _*)))
      case _      => ()
    }
  }

  /** Returns a [[Validation]] with the result of applying this [[Validator]] to
    * the specified value.
    */
  override def apply(value: T): Validation = {
    verify(value).toList match {
      case Nil     => Success
      case h :: ts => Failure(NonEmptyList(h, ts: _*))
    }
  }

  /** Returns a [[Validation]] with the result of applying this [[Validator]] to
    * the specified value. Alias of [[apply]].
    *
    * Useful to disambiguate the implicit `apply` and the last application group
    * with implicit values that some Validator builders expect. E.g:
    *
    * {{{
    *   object Validator {
    *     ...
    *     def min[T](min: T, inclusive: Boolean = false)(errorCode: RnbErrorCode)(implicit o: Ordering[T]): Validator[T] = ???
    *     ...
    *   }
    *
    *   // usage:
    *   validate(
    *     Validator.min(1)(someErrorCode)(theValue)    // nope, this doesn't work because after the errorCode the implicit ordering is expected
    *     Validator.min(1)(someErrorCode).on(theValue) // this works because the ordering is applied implicitly before calling `on` so there is no ambiguity
    *   )
    * }}}
    */
  def on(value: T): Validation = apply(value)

  // operations

  /** Combines this [[Validator]] with another one expecting the same value
    * type. In case the value passes this [[Validator]], returns the result of
    * `other.on(theValue)`
    */
  def &&(other: Validator[T]): Validator[T] = { value =>
    LazyList(verify _, other.verify _)
      .map(_.apply(value))
      .find(_.nonEmpty)
      .getOrElse(Nil)
  }

  /** Combines this [[Validator]] with another one expecting the same value
    * type. The new [[Validator]] will return the sum of errors from both
    * original [[Validator]] s.
    */
  def ++(other: Validator[T]): Validator[T] = { value => verify(value) ++ other.verify(value) }

  /** Adapts the input value of this [[Validator]] */
  def contramap[U](f: U => T): Validator[U] = { value => verify(f(value)) }

  /** Lifts this [[Validator]] to accept an [[Option]] of the validated value.
    * Pass if the validated value is [[None]] or [[Some]] (concreteValue) and
    * concreteValue passes the original [[Validator]].
    */
  def opt: Validator[Option[T]] = _.fold(NoError)(verify)

  /** Lifts this [[Validator]] to accept a [[Seq]] of values to validate. Pass
    * if each element of the [[Seq]] passes the original [[Validator]].
    */
  def seq: Validator[Seq[T]] = _.map(verify).find(_.nonEmpty).getOrElse(NoError)
}

/** Useful [[Validator]] builders for generic validations */
object Validator {

  /** The result of applying a [[Validator]] to some value. We are only
    * interested on the possible validation errors, so the value's type is not
    * relevant here.
    */
  type Validation = ValidationNel[ErrorCode, _]

  /** The unlifted version of a [[Validator]]; a function expecting a value and
    * returning the [[Validation]] result
    */
  type ValidatorF[T] = T => Validation

  // to avoid explicitly typing None as an Option[RnbErrorCode] each time
  private val NoError: Seq[ErrorCode] = Nil

  /** A successful [[Validation]] */
  val Success: Validation = ().successNel

  /** Lifts a [[LegacyValidator]] (a function expecintg a value to validate and
    * the errorCode in case it doesn't pass, and returning a [[Validation]])
    * into a [[Validator]]
    */
  def apply[T](ffValidator: (T, ErrorCode) => Validation)(implicit errorCode: ErrorCode): Validator[T] = {
    ffValidator(_, errorCode).fold(_ => Seq(errorCode), _ => Nil)
  }

  /* ***************************************************************************
   *
   * NOTE: Add unit tests for any new validator you put in here.
   *
   * ************************************************************************* */

  /** A [[Validator]] that always passes */
  def pass[T]: Validator[T] = _ => NoError

  /** A [[Validator]] that always fails */
  def fail[T](errorCode: ErrorCode*): Validator[T] = _ => errorCode

  /** Returns a [[Validator]] that passes when the validated [[BigDecimal]] fits
    * the maximum precision stablished by `maxDecimalLength`. It only considers
    * as many significant figures as `sigfigs` specifies.
    *
    * Examples:
    *   - {{{bigDecimal(3, 10).on(1.12345600005)          // passes because 1.12 has only 2 decimal figures}}}
    *   - {{{bigDecimal(3, 10).on(0.0000518123123123123)  // passes because 0.0000518 has only 7 decimal figures}}}
    *   - {{{bigDecimal(5, 12).on(7.9855555555E-8)        // passes because 7.9856E-8 (0.000000079856) has exactly 12 decimal figures}}}
    *   - {{{bigDecimal(8, 12).on(7.9855555555E-8)        // fails because 7.9855556E-8 (0.000000079855556) has more than 12 decimal figures (it has 15, 7 zeros and 8 sig figs)}}}
    *
    * Note: both `sigfigs` and `maxDecimalLength` must be greater than Zero,
    * otherwise an [[IllegalArgumentException]] will be thrown.
    *
    * @param sigfigs
    *   number of significant figures to consider. MUST be greater than Zero
    * @param maxDecimalLength
    *   number of figures to the right of the decimal point. MUST be greater
    *   than Zero
    */
  def decimalPrecision(sigfigs: Int, maxDecimalLength: Int)(implicit e: ErrorCode): Validator[BigDecimal] = {
    require(sigfigs > 0, "'sigfigs' must be greater than 0")
    require(maxDecimalLength > 0, "'maxDecimalLength' must be greater than 0")

    onCondition { value =>
      // if after multiplying by maxDecimalLength we get a non decimal number it means it has fewer or equal number of decimal figures than maxDecimalLength
      (value.round(new MathContext(sigfigs)) * BigDecimal(s"1E$maxDecimalLength")).isWhole
    }
  }

  /** Returns a [[Validator]] that passes when the validated values are all
    * different
    */
  def distinct(implicit e: ErrorCode): Validator[Seq[_]] = onCondition(values => values.size == values.distinct.size)

  /** Returns a [[Validator]] that passes when the validated string a valid
    * `scala.concurrent.duration.Duration`
    */
  def duration()(implicit e: ErrorCode): Validator[String] = Validator(LegacyValidator.duration)

  /** Returns a [[Validator]] that passes when the validated string matches the
    * specified regex pattern
    */
  def matches(regex: Regex)(implicit e: ErrorCode): Validator[String] = Validator(LegacyValidator.regex(_, regex, _))

  /** Returns a [[Validator]] that passes when the validated value is less than
    * the specified max value
    */
  def max[T](max: T, inclusive: Boolean = true)(errorCode: ErrorCode)(implicit o: Ordering[T]): Validator[T] = {
    val lower = if (inclusive) o.lteq _ else o.lt _
    onCondition[T](lower(_, max))(errorCode)
  }

  /** Returns a [[Validator]] that passes when the validated value is no less
    * than the specified min value
    */
  def min[T](min: T, inclusive: Boolean = true)(errorCode: ErrorCode)(implicit o: Ordering[T]): Validator[T] = {
    val greater = if (inclusive) o.gteq _ else o.gt _
    onCondition[T](greater(_, min))(errorCode)
  }

  /** Returns a [[Validator]] that passes when the validated string is non blank
    */
  def nonBlank(implicit e: ErrorCode): Validator[String] = Validator(nonBlankString)

  /** Returns a [[Validator]] that passes when the validated Seq is non empty */
  def nonEmpty[T](implicit e: ErrorCode): Validator[Seq[T]] = onCondition(_.nonEmpty)

  /** Returns a [[Validator]] that passes when `condition` is true when applied
    * with the validated value
    */
  def onCondition[T](condition: T => Boolean)(implicit e: ErrorCode): Validator[T] = Validator((value, errorCode) => if (condition(value)) Success else errorCode.failureNel)

  /** Returns a [[Validator]] that passes when `op` success when applied with
    * the validated value
    */
  def onSuccess[T](op: T => Try[_])(implicit e: ErrorCode): Validator[T] = Validator((value, errorCode) => op(value).fold(_ => errorCode.failureNel, _ => Success))

  /** Returns a [[Validator]] that passes when the validated value is between an
    * upper and lower bounds (inclusive).
    */
  def range[T](min: T, max: T, inclusive: Boolean = true)(errorCode: ErrorCode)(implicit o: Ordering[T]): Validator[T] = {
    val (greater, lower) = if (inclusive) (o.gteq _, o.lteq _) else (o.gt _, o.lt _)
    onCondition[T](value => greater(value, min) && lower(value, max))(errorCode)
  }

  /* ***************************************************************************
   *
   * NOTE: Add unit tests for any new validator you put in here.
   *
   * ************************************************************************* */
}
