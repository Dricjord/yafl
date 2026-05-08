package yafl.typer

import yafl.Diagnostic
import yafl.syntax.{Syntax, TermTree, TypeTree}
import yafl.typer.Type

object Typer:

  /** A typing environment, mapping a term variable to its type. */
  opaque type Context = Map[String, Type]

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

  /** Returns the type of `syntax`, throwing an exception if it is ill-typed. */
  def check(syntax: Syntax[TermTree])(using Context): Type =
    syntax.value match
      case e: TermTree.Variable =>
        context.get(e.name) match
          case Some(t) => t
          case _ => throw Diagnostic.undefinedSymbol(e.name, syntax.span)

      case TermTree.UnitLiteral =>
        Type.Ground.Unit

      case e: TermTree.BooleanLiteral =>
        Type.Ground.Bool

      case e: TermTree.IntegerLiteral =>
        Type.Ground.Int

      case e: TermTree.TermApplication =>
        val t = check(e.abstraction)
        val u = check(e.argument)
        t match
          case Type.Arrow(a, b) if a == u =>
            b
          case Type.Arrow(a, _) =>
            throw Diagnostic(
              s"found '${u}', expected '${a}'", e.argument.span)
          case _ =>
            throw Diagnostic(
              s"cannot apply value of type '${t}' to argument of type '${u}'", syntax.span)

      case _ =>
        ???

  /** Returns the current typing environment. */
  private def context(using ctx: Context): Context =
    ctx

end Typer
