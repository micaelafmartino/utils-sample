package samples.utils

import dependencies.{CustomRootException, ErrorCode}

/** Common extractors (objects with an `unapply`) which make Pattern Matching
  * easier/cleaner.
  */
package object extractors {

  /** Extracts the cause of a [[Throwable]] */
  object ThrowableCause {
    def unapply(t: Throwable): Option[Throwable] = Option(t.getCause)
  }

  object RootException {

    /** Useful to test the [[ErrorCode]] of a [[CustomRootException]].
      *
      * Usage:
      * {{{
      *   val exception: CustomRootException = ???
      *   exception match {
      *     case CustomRootException(ErrorCode.SomeCode)      => ???
      *     case CustomRootException(ErrorCode.SomeOtherCode) => ???
      *     case _                                            => ???
      *   }
      * }}}
      */
    def unapply(e: CustomRootException): Option[ErrorCode] = ErrorCode.valueOf(e.exceptionMessage)
  }
}
