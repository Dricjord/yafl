
import yafl.SourceFile
import yafl.optimizer.Optimizer
import yafl.parser.Parser
import yafl.syntax.{Syntax, TermTree}
import yafl.typer.{TypedProgram, Typer}

final class OptimizerTests extends munit.FunSuite:

  test("constant folding"):
    val optimized = optimize("1 + 2 + 3")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(6) => ()

  test("normalization"):
      import TermTree.TermApplication as F
      import TermTree.Binding as B
      // On utilise '#argv 0' qui est une valeur d'exécution, empêchant la propagation de constante pure
      val optimized = optimize("let x = #argv 0; x + 1")
      (optimized.syntax.value : @unchecked) match
        case B(_, _, Syntax(F(lhs, Syntax(TermTree.Variable("x"), _)), _)) =>
          (lhs.value : @unchecked) match
            case F(_, Syntax(TermTree.IntegerLiteral(1), _)) => ()

  test("dead code elimination - conditional true"):
    // Devrait éliminer la branche 'else' et ne garder que '1'
    val optimized = optimize("if true then 1 else 2 * 3")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(1) => ()

  test("dead code elimination - conditional false"):
    // Devrait éliminer la branche 'then' et ne garder que '2 * 3'
    // Note : si le constant folding passe avant, '2 * 6' pourrait être réduit à '6'
    val optimized = optimize("if false then 1 else 6")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(6) => ()

  test("dead code elimination - unused let binding"):
    // La variable 'x' n'est pas utilisée dans le corps '1', le let doit disparaître
    val optimized = optimize("let x = 2; 1")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(1) => ()
    
  test("constant propagation and dead code elimination"):
    // 'x' est propagé, 'x + 3' devient '2 + 3' (constant foldé à 5).
    // Ensuite, le let x devient mort et est supprimé par le DCE.
    val optimized = optimize("let x = 2; x + 3")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(5) => ()
  /** Compiles `input` to a WebAssembly module and returns an instance of it. */
  private def optimize(input: String): TypedProgram =
    Optimizer.optimize(Typer.check(Parser.parse(SourceFile("test", input))))

end OptimizerTests
