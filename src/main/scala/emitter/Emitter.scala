package yafl.emitter

import yafl.syntax.{InfixOperator, Syntax, TermTree}
import yafl.typer.{Type, TypedProgram}
import yafl.{Diagnostic, Rope}

object Emitter:

  /** The context in which code generation is taking place.
    *
    * @param types A map from each term to its type.
    * @param locals The WAT local variables declared so far, as (name, type) pairs. Appended in
    *   declaration order so that no reversal is needed when emitting them.
    * @param nextId A monotonically increasing counter used to produce unique WAT local names.
    */
  case class Context(
    types: Map[Syntax[TermTree], Type],
    locals: List[(String, String)] = Nil,
    nextId: Int = 0
  )

  /** The result of generating the code of an expression. */
  type Result[+T] = yafl.Result[T, Context]

  /** Returns the WAT value type */
  private def watTypeOf(tpe: Type, site: Syntax[?]): String =
    tpe match
      case Type.Ground.Bool | Type.Ground.Int => "i32"
      case u => throw Diagnostic(s"unsupported type '${u}'", site.span)

  /** Declares a fresh WAT local variable of `wasmType` for a Yafl binding named `name`, returning
    * its unique WAT name. The counter is advanced and the declaration is appended to the context so
    * that the emitter encounters them in source order.
    */
  private def declareLocal(name: String, wasmType: String)(using Context): Result[String] =
    val watName = s"${name}_${context.nextId}"
    yafl.Result(watName)(using context.copy(
      locals = context.locals :+ (watName, wasmType),
      nextId = context.nextId + 1
    ))

  /** Returns code of `program`. */
  def emit(program: TypedProgram): String =
    val main = emitMain(program.syntax)(using Context(program.types))
    s"(module (memory $$__m 1) ${argc} ${argv} ${main.value})"

  /** The code of the built-in `argc` function. */
  private val argc: String =
    """
    (func $#argc (result i32)
      (i32.const 0x0000)
      (i32.load)
    )
    """.stripIndent

  /** The code of the built-in `argv` function. */
  private val argv: String =
    """
    (func $#argv (param $i i32) (result i32)
      (i32.const 4) (local.get $i) (i32.mul)
      (i32.const 4) (i32.add)
      (i32.load)
    )
    """.stripIndent

  /** Returns the code of the main function. */
  private def emitMain(body: Syntax[TermTree])(using Context): Result[Rope] = {
    val output = watTypeOf(context.types(body), body)

    emitAsValue(body, Map.empty).and { code =>
      // context.locals is already in declaration order (appended, not prepended).
      val decls = context.locals.map { case (n, t) => s"(local $$$n $t)" }.mkString(" ")
      val prefix = if decls.isEmpty then "" else s"$decls "
      result(Rope(s"(func (export \"main\") (result ${output}) ${prefix}") ++ code ++ ")")
    }
  }

  /** Returns the code computing the value expressed by `tree`, which occurs as an argument or a
    * return value.  */
  private def emitAsValue(tree: Syntax[TermTree], env: Map[String, String])(using Context): Result[Rope] = {
    tree.value match
      case TermTree.Variable(n) =>
        // Built-in symbols require special handling. Specifically, `#argc` must be emitted as a
        // call, since it has the type of a constant, and `#argv` must be emitted as a closure,
        // using eta-expansion. Other symbols can be emitted as local loads.
        n match
          case "#argc" => result(Rope(s"(call $$#argc)"))
          case "#argv" => ???
          case _ => result(Rope(s"(local.get $$${env.getOrElse(n, n)})"))

      case TermTree.IntegerLiteral(n) =>
        result(Rope(s"(i32.const ${n})"))

      case TermTree.BooleanLiteral(n) =>
        result(Rope(s"(i32.const ${if n then 1 else 0})"))

      case TermTree.Binding(nameNode, initializer, body) =>
        val name = nameNode.value.name
        val wasmType = watTypeOf(context.types(initializer), initializer)

        declareLocal(name, wasmType).and { watName =>
          emitAsValue(initializer, env).and { initCode =>
            emitAsValue(body, env + (name -> watName)).map { bodyCode =>
              initCode ++ s"(local.set $$$watName) " ++ bodyCode
            }
          }
        }

      case TermTree.TermApplication(callee, a) => callee.value match
        case TermTree.TermApplication(InfixOperator(f), b) =>
          emitAsValue(b, env).and { lhs => emitAsValue(a, env).map { rhs =>
            val operation = f match
              case InfixOperator.Add => "(i32.add)"
              case InfixOperator.Sub => "(i32.sub)"
              case InfixOperator.Mul => "(i32.mul)"
              case InfixOperator.Div => "(i32.div_s)"
              case InfixOperator.Eq  => "(i32.eq)"
              case InfixOperator.Neq => "(i32.ne)"
              case InfixOperator.Lt  => "(i32.lt_s)"
              case InfixOperator.Lte => "(i32.le_s)"
              case InfixOperator.Gt  => "(i32.gt_s)"
              case InfixOperator.Gte => "(i32.ge_s)"
              case InfixOperator.And => "(i32.and)"
              case InfixOperator.Or  => "(i32.or)"
            lhs ++ rhs ++ operation
          }}

        case _ =>
          emitAsCallee(callee, env).and { f => emitAsValue(a, env).map { x => x ++ f } }

      case _ =>
        throw Diagnostic("unsupported term", tree.span)
  }

  /** Returns the code computing the value expressed by `tree`, which occurs as the term being
    * applied in a term application.
    *
    * The result has the form `(call f)` where `f` is a local function or `(call_indirect t i)`
    * where `t` is a type and `i` is the index in the function table.
    */
  private def emitAsCallee(tree: Syntax[TermTree], env: Map[String, String])(using Context): Result[Rope] = {
    tree.value match
      case TermTree.Variable("#argv") =>
        result(Rope(s"(call $$#argv)"))
      case _ =>
        ???
  }

  /** Returns the current context. */
  private def context(using ctx: Context): Context =
    ctx

  /** Returns a result wrapping `value` together with the current context. */
  private def result[T](value: T)(using Context): Result[T] =
    yafl.Result(value)

end Emitter
