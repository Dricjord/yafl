package yafl.parser

import yafl.{SourceFile, SourceSpan}

object Lexer:

  /** A regular expression matching newlines. */
  val newline = "\\R".r

  /** Returns `true` iff `c` is a letter or the underscore (i.e., `_`). */
  def isLetterOrUnderscore(c: Char): Boolean =
    c == '_' || c.isLetter

  /** Returns `true` iff `c` is a decimal digit. */
  def isDecimalDigit(c: Char): Boolean =
    c.isDigit

  /** Returns `true` iff `c` may be part of an operator. */
  def isOperator(c: Char): Boolean =
    "!|&<>=+-*/".contains(c)

end Lexer

extension (source: SourceFile)

  /** Returns the next token in `source` from `start` or `None` if there isn't any. */
  def nextToken(start: SourceFile.Index): Option[Token] = {
    // Ignore whitespaces and comments.
    val position = discardWhitespacesAndComments(start)

    // Read a single token unless there's nothing left in the input.
    if position >= source.end then
      None
    else if source.text(position) == '#' then
      Some(takeBuiltinIdentifier(position))
    else if Lexer.isLetterOrUnderscore(source.text(position)) then
      Some(takeKeywordOrIdentifier(position))
    else if Lexer.isDecimalDigit(source.text(position)) then
      Some(takeIntegerLiteral(position))
    else if Lexer.isOperator(source.text(position)) then
      Some(takeOperator(position))
    else
      Some(takePunctuationOrError(position))
  }

  /** Returns the character at index `position` in source or `None` iff `position` is in bounds. */
  private def peek(position: SourceFile.Index): Option[Char] =
    if position != source.end then Some(source.text(position)) else None

  /** Returns the position immediately after all whitespaces and comments preceding the next token
    * in `source` from `start`.
    */
  private def discardWhitespacesAndComments(start: SourceFile.Index): SourceFile.Index =
    if (start != source.end) && source.text(start).isWhitespace then
      discardWhitespacesAndComments(start + 1)
    else if takePrefix(start, "//").isDefined then
      Lexer.newline.findFirstMatchIn(source.text.subSequence(start, source.end)) match
        case Some(m) => discardWhitespacesAndComments(start + m.end)
        case _ => source.end
    else
      start

  /** Returns the position immediately after longest string in `source` from `start` whose
    * characters all satisfy `predicate`.
    */
  private def takeWhile(start: SourceFile.Index)(predicate: Char => Boolean): SourceFile.Index =
    if (start != source.end) && predicate(source.text(start)) then
      takeWhile(start + 1)(predicate)
    else
      start

  /** If the contents of `source` from `start` starts with `prefix`, returns the position
    * immediately after `prefix`; otherwise, returns `None`.
    */
  private def takePrefix(start: SourceFile.Index, prefix: String): Option[SourceFile.Index] =
    def loop(i: Int, j: Int): Option[SourceFile.Index] =
      if i == prefix.length then
        Some(j)
      else if (j != source.end) && (prefix(i) == source.text(j)) then
        loop(i + 1, j + 1)
      else
        None
    loop(0, start)

  /** Reads a built-in identifier in `source` from `start`. */
  private def takeBuiltinIdentifier(start: SourceFile.Index): Token =
    val end = takeWhile(start + 1)(Lexer.isLetterOrUnderscore)
    if (source.text(start) != '#') || (end == (start + 1)) then
      Token(Token.error, SourceSpan(start, end, source))
    else
      Token(Token.identifier, SourceSpan(start, end, source))

  /** Reads a keyword or identifier in `source` from `start`. */
  private def takeKeywordOrIdentifier(start: SourceFile.Index): Token =
    val end = takeWhile(start)(Lexer.isLetterOrUnderscore)
    assert(start < end)
    val tag: Token.Tag = source.text.subSequence(start, end) match
      case "_" => Token.underscore
      case "true" => Token.boolean
      case "false" => Token.boolean
      case "if" => Token.`if`
      case "then" => Token.`then`
      case "else" => Token.`else`
      case "fix" => Token.fix
      case "let" => Token.let
      case _ => Token.identifier
    Token(tag, SourceSpan(start, end, source))

  /** Reads an integer literal in `source` from `start`. */
  private def takeIntegerLiteral(start: SourceFile.Index): Token =
    val end = takeWhile(start)(Lexer.isDecimalDigit)
    assert(start < end)
    Token(Token.integer, SourceSpan(start, end, source))

  /** Reads an operator in `source` from `start`. */
  private def takeOperator(start: SourceFile.Index): Token =
    val end = takeWhile(start)(Lexer.isOperator)
    assert(start < end)
    val tag: Token.Tag = source.text.subSequence(start, end) match
      case "=" => Token.equal
      case "->" => Token.thinArrow
      case "=>" => Token.thickArrow
      case  _ => Token.operator
    Token(tag, SourceSpan(start, end, source))


  /** Reads a punctuation token in `source` from `start` or reads a single character as an error
    * if that character is not punctuation.
    *
    * - Requires: `start` is less than `source.end`.
    */
  private def takePunctuationOrError(start: SourceFile.Index): Token =
    val tag: Token.Tag = source.text(start) match
      case '[' => Token.leftBracket
      case ']' => Token.rightBracket
      case '(' => Token.leftParenthesis
      case ')' => Token.rightParenthesis
      case '.' => Token.dot
      case ',' => Token.comma
      case ';' => Token.semicolon
      case ':' => Token.colon
      case  _ => Token.error
    Token(tag, SourceSpan(start, start + 1, source))
