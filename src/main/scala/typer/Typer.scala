package yafl.typer

import yafl.Diagnostic
import yafl.syntax.{Syntax, TermTree, TypeTree}
import yafl.typer.Type

object Typer:

  /** The context in which type checking is taking place.
    *
    * @param bindings A map from local variable to its type.
    * @param types A map from a term to its type.
    */
  case class Context(bindings: Map[String, Type], types: Map[Syntax[TermTree], Type]):

    /** Returns a copy of `this` in which the type of `e` is defined as `t`. */
    def assigning(e: Syntax[TermTree], t: Type): Context =
      copy(types = types.updated(e, t))

  object Context:

    /** A typing context containing the symbols of the standard library. */
    def builtin: Context = {
      val bindings = Map[String, Type](
        "infix+" -> Type.Arrow.binary(Type.Ground.Int, Type.Ground.Int, Type.Ground.Int),
        "infix-" -> Type.Arrow.binary(Type.Ground.Int, Type.Ground.Int, Type.Ground.Int),
        "infix*" -> Type.Arrow.binary(Type.Ground.Int, Type.Ground.Int, Type.Ground.Int),
        "infix/" -> Type.Arrow.binary(Type.Ground.Int, Type.Ground.Int, Type.Ground.Int),
      )
      Context(bindings, Map())
    }

  end Context

  /** The result of type checking an expression. */
  type Result[+T] = yafl.Result[T, Context]

  /** Type checks program and returns a map from each (sub)term to its type. */
  def check(program: Syntax[TermTree]): Map[Syntax[TermTree], Type] =
    val typed = typeOf(program)(using Context.builtin)
    typed.state.types

  /** Returns the type of `term` in an updated context mapping `term` to that type. */
  private def typeOf(term: Syntax[TermTree])(using Context): Result[Type] = {
    val found: Result[Type] = term.value match {
      case e: TermTree.Variable =>
        context.bindings.get(e.name) match
          case Some(t) => result(t)
          case _ => throw Diagnostic.undefinedSymbol(e.name, term.span)

      case TermTree.UnitLiteral =>
        result(Type.Ground.Unit)

      case e: TermTree.BooleanLiteral =>
        result(Type.Ground.Bool)

      case e: TermTree.IntegerLiteral =>
        result(Type.Ground.Int)

      case e: TermTree.TermApplication =>
        typeOf(e.abstraction).andCombine(typeOf(e.argument)).map { (t, u) =>
          t match
          case Type.Arrow(a, b) if a == u =>
            b
          case Type.Arrow(a, _) =>
            throw Diagnostic(
              s"found '${u}', expected '${a}'", e.argument.span)
          case _ =>
            throw Diagnostic(
              s"cannot apply value of type '${t}' to argument of type '${u}'", term.span)
        }

      case _ =>
        ???
    }

    result(found.value)(using found.state.assigning(term, found.value))
  }

  /** Returns the current context. */
  private def context(using ctx: Context): Context =
    ctx

  /** Returns a result wrapping `value` together with the current context. */
  private def result[T](value: T)(using Context): Result[T] =
    yafl.Result(value)

end Typer
