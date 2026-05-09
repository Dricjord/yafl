package yafl.parser

/** A terminal symbol of the syntactic grammar.
  *
  * @param tag The tag of the token.
  * @param span The site from which the token was extracted.
  */
case class Token(tag: Token.Tag, span: yafl.SourceSpan):

  /** Returns the text from which this token was extracted. */
  def text: CharSequence =
    span.text

  /** Returns `true` if the tag of this token is contained in `ks`. */
  def isAnyOf(ks: Token.Tag*): Boolean =
    ks.contains(tag)

  /** Returns `true` iff `this` delimits the end of a term. */
  def isDelimiter: Boolean =
    isAnyOf(Token.rightParenthesis, Token.colon, Token.semicolon, Token.`then`, Token.`else`)

  /** If `this` is an operator, returns its precedence; otherwise, returns -1. */
  def precedence: Int =
    if tag == Token.operator then text match
      case "*" | "/" =>
        500
      case "+" | "-" =>
        400
      case "==" | "!=" | "<" | "<=" | ">=" | ">" =>
        300
      case "&&" =>
        200
      case "||" =>
        100
      case _ =>
        0
    else
      -1

object Token:

  /** The tag of a token. */
  opaque type Tag = Byte

  val error             : Tag = 0x00
  val boolean           : Tag = 0x01
  val integer           : Tag = 0x02
  val underscore        : Tag = 0x03
  val identifier        : Tag = 0x04
  val `if`              : Tag = 0x05
  val `then`            : Tag = 0x06
  val `else`            : Tag = 0x07
  val fix               : Tag = 0x08
  val let               : Tag = 0x09
  val equal             : Tag = 0x0a
  val operator          : Tag = 0x0b
  val thinArrow         : Tag = 0x0c
  val thickArrow        : Tag = 0x0d
  val dot               : Tag = 0x0e
  val comma             : Tag = 0x0f
  val colon             : Tag = 0x10
  val semicolon         : Tag = 0x11
  val leftBracket       : Tag = 0x12
  val rightBracket      : Tag = 0x13
  val leftParenthesis   : Tag = 0x14
  val rightParenthesis  : Tag = 0x15

  /** Returns a closure that accepts a token and returns `true` iff that token has tag `k`. */
  def hasTag(k: Tag): Token => Boolean =
    (t: Token) => t.tag == k

end Token
