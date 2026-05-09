import yafl.SourceFile
import yafl.parser.Parser
import yafl.syntax.{Syntax, TermTree, TypeTree}
import yafl.syntax.TermTree.TypeApplication
import yafl.syntax.TypeTree.Arrow

final class ParserTests extends munit.FunSuite:

  test("unit"):
    assertEquals(parse("()").value, TermTree.UnitLiteral)

  test("bool"):
    assertEquals(parse("true").value, TermTree.BooleanLiteral(true))
    assertEquals(parse("false").value, TermTree.BooleanLiteral(false))

  test("integer"):
    (parse("42") : @unchecked) match
      case Syntax(TermTree.IntegerLiteral(n), _) =>
        assertEquals(n, 42)

  test("identifier"):
    assertEquals(parse("foo").value, TermTree.Variable("foo"))
    assertEquals(parse("#argc").value, TermTree.Variable("#argc"))

  test("infix"):
    import TermTree.TermApplication as A
    (parse("1 + 2 * -3") : @unchecked) match
      case Syntax(A(Syntax(A(add, one), _), Syntax(A(x, y), _)), _) =>
        assertEquals(add.value, TermTree.Variable("infix+"))
        assertEquals(one.value, TermTree.IntegerLiteral(1))
        (x : @unchecked) match
          case Syntax(A(mul, two), _) =>
            assertEquals(mul.value, TermTree.Variable("infix*"))
            assertEquals(two.value, TermTree.IntegerLiteral(2))
        (y : @unchecked) match
          case Syntax(A(minus, three), _) =>
            assertEquals(minus.value, TermTree.Variable("prefix-"))
            assertEquals(three.value, TermTree.IntegerLiteral(3))

  test("prefix"):
    import TermTree.TermApplication as A
    (parse("- + 1") : @unchecked) match
      case Syntax(A(x, Syntax(A(y, z), _)), _) =>
        assertEquals(x.value, TermTree.Variable("prefix-"))
        assertEquals(y.value, TermTree.Variable("prefix+"))
        assertEquals(z.value, TermTree.IntegerLiteral(1))

  test("term abstraction"):
    import TermTree.TermAbstraction as A
    (parse("(x : A) => x") : @unchecked) match
      case Syntax(A(x, t, b), _) =>
        assertEquals(x.value.name, "x")
        assertEquals(t.value, TypeTree.Variable("A"))
        assertEquals(b.value, TermTree.Variable("x"))

  test("term application"):
    import TermTree.TermApplication as A
    (parse("x y") : @unchecked) match
      case Syntax(A(x, y), _) =>
        assertEquals(x.value, TermTree.Variable("x"))
        assertEquals(y.value, TermTree.Variable("y"))

  test("type abstraction"):
    import TermTree.TypeAbstraction as A
    (parse("[A] => x") : @unchecked) match
      case Syntax(A(a, x), _) =>
        assertEquals(a.value.name, "A")
        assertEquals(x.value, TermTree.Variable("x"))

  test("type application"):
    import TermTree.TypeApplication as A
    (parse("x y [A] [B]") : @unchecked) match
      case Syntax(TermTree.TermApplication(x, Syntax(A(Syntax(A(y, a) ,_), b), _)), _) =>
        assertEquals(x.value, TermTree.Variable("x"))
        assertEquals(y.value, TermTree.Variable("y"))
        assertEquals(a.value, TypeTree.Variable("A"))
        assertEquals(b.value, TypeTree.Variable("B"))

  test("recursive abstraction"):
    (parse("fix f : A -> A = b") : @unchecked) match
      case Syntax(TermTree.RecursiveAbstraction(f, t, b), _) =>
        assertEquals(f.value.name, "f")
        assertEquals(b.value, TermTree.Variable("b"))
        assert(t.value.isInstanceOf[TypeTree.Arrow])

  test("ill-formed recursive abstraction"):
    interceptMessage("expected ':'")(parse("fix f = b"))
    interceptMessage("expected '='")(parse("fix f : T b"))

  test("conditional"):
    (parse("if x then y else z") : @unchecked) match
      case Syntax(TermTree.Conditional(x, y, z), _) =>
        assertEquals(x.value, TermTree.Variable("x"))
        assertEquals(y.value, TermTree.Variable("y"))
        assertEquals(z.value, TermTree.Variable("z"))

  test("ill-formed conditional"):
    interceptMessage("expected 'then'")(parse("if a"))
    interceptMessage("expected 'else'")(parse("if a then b"))

  test("parenthesized term"):
    (parse("(((x)))") : @unchecked) match
      case Syntax(x, _) =>
        assertEquals(x, TermTree.Variable("x"))

  test("binding"):
    (parse("let x = y; z") : @unchecked) match
      case Syntax(TermTree.Binding(x, y, z), _) =>
        assertEquals(x.value.name, "x")
        assertEquals(y.value, TermTree.Variable("y"))
        assertEquals(z.value, TermTree.Variable("z"))

  test("ill-formed binding"):
    interceptMessage("expected '='")(parse("let x"))
    interceptMessage("expected ';'")(parse("let x = y"))

  test("arrow"):
    import TypeTree.Arrow as A
    (parse("x [A -> B -> C]") : @unchecked) match
      case Syntax(TermTree.TypeApplication(_, Syntax(A(a, Syntax(A(b, c), _)), _)), _) =>
        assertEquals(a.value, TypeTree.Variable("A"))
        assertEquals(b.value, TypeTree.Variable("B"))
        assertEquals(c.value, TypeTree.Variable("C"))

  test("universal type"):
    import TypeTree.ForAll as F
    (parse("x [[A] => B]") : @unchecked) match
      case Syntax(TermTree.TypeApplication(_, Syntax(F(a, b), _)), _) =>
        assertEquals(a.value.name, "A")
        assertEquals(b.value, TypeTree.Variable("B"))

  test("parenthesized type"):
    import TypeTree.Arrow as A
    (parse("x [(A -> B) -> C]") : @unchecked) match
      case Syntax(TermTree.TypeApplication(_, Syntax(A(Syntax(A(a, b), _), c), _)), _) =>
        assertEquals(a.value, TypeTree.Variable("A"))
        assertEquals(b.value, TypeTree.Variable("B"))
        assertEquals(c.value, TypeTree.Variable("C"))

  test("multiple term parameters"):
    import TermTree.TermAbstraction as A
    (parse("(x : A, y : B, z : C) => 0") : @unchecked) match
      case Syntax(A(x, a, Syntax(A(y, b, Syntax(A(z, c, _), _)), _)), _) =>
        assertEquals(x.value.name, "x")
        assertEquals(a.value, TypeTree.Variable("A"))
        assertEquals(y.value.name, "y")
        assertEquals(b.value, TypeTree.Variable("B"))
        assertEquals(z.value.name, "z")
        assertEquals(c.value, TypeTree.Variable("C"))

  test("multiple type parameters"):
    import TermTree.TypeAbstraction as A
    (parse("[A, B, C] => x") : @unchecked) match
      case Syntax(A(a, Syntax(A(b, Syntax(A(c, x), _)), _)), _) =>
        assertEquals(a.value.name, "A")
        assertEquals(b.value.name, "B")
        assertEquals(c.value.name, "C")
        assertEquals(x.value, TermTree.Variable("x"))

  test("multiple type arguments"):
    import TermTree.TypeApplication as A
    (parse("x [A, B]") : @unchecked) match
      case Syntax(A(Syntax(A(x, a), _), b), _) =>
        assertEquals(x.value, TermTree.Variable("x"))
        assertEquals(a.value, TypeTree.Variable("A"))
        assertEquals(b.value, TypeTree.Variable("B"))

  test("multiple type variables"):
    import TypeTree.ForAll as F
    (parse("x [[A, B] => C]") : @unchecked) match
      case Syntax(TermTree.TypeApplication(_, t), _) =>
        (t : @unchecked) match
          case Syntax(F(a, Syntax(F(b, c), _)), _) =>
            assertEquals(a.value.name, "A")
            assertEquals(b.value.name, "B")
            assertEquals(c.value, TypeTree.Variable("C"))

  test("missing parentheses"):
    interceptMessage("expected ')'")(parse("(x"))
    interceptMessage("expected ')'")(parse("(x : a => x"))
    interceptMessage("expected ')'")(parse("x [(a]"))

  test("missing brackets"):
    interceptMessage("expected ']'")(parse("[a => x"))
    interceptMessage("expected ']'")(parse("x [a"))
    interceptMessage("expected ']'")(parse("x [[a => a]"))

  test("missing arrows"):
    interceptMessage("expected '=>'")(parse("(x : a) x"))
    interceptMessage("expected '=>'")(parse("[a] x"))
    interceptMessage("expected '=>'")(parse("x [[a] a]"))

  /** Returns the syntax tree parsed from `input`. */
  private def parse(input: String): Syntax[TermTree] =
    Parser.parse(SourceFile("test", input))

end ParserTests
