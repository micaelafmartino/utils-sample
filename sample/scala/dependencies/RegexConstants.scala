package dependencies

/** A container for regex used across multiple services */
case object RegexConstants {

  /** <ul> <li>lower case letters</li> <li>upper case letters</li>
    * <li>numbers</li> <li>spaces " "</li> <li>dashes "-"</li> </ul>
    */
  final val ValidNameRegex = "[a-zA-Z0-9 -]*".r

  /** <ul> <li>starts with a letter</li> <li>lower case letters</li> <li>upper
    * case letters</li> <li>numbers</li> <li>length at least 1 character</li>
    * </ul>
    */
  final val GraphqlCompliantKeyRegex = "^[a-zA-Z]+[a-zA-Z0-9]*".r

  /** <ul> <li>lower case letters</li> <li>upper case letters</li>
    * <li>numbers</li> <li>dashes "-"</li> </ul>
    */
  final val IdRegex = "^[a-zA-Z0-9-]*".r

  /** <ul> <li>lower case letters</li> <li>upper case letters</li>
    * <li>numbers</li> <li>dashes "-"</li> <li>underscores "_"</li> </ul>
    */
  final val AlphanumericWithDashAndUnderscores = "^[a-zA-Z0-9-_]*".r

  /** <ul> <li>lower case letters</li> <li>upper case letters</li>
    * <li>numbers</li> </ul>
    */
  final val AlphaNumericRegex = "[a-zA-Z0-9]*".r
}
