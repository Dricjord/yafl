package yafl.optimizer

import yafl.syntax.{InfixOperator, Syntax, TermTree}
import yafl.typer.{Type, TypedProgram}

object Optimizer:

  /** Returns `program` optimized. */
  def optimize(program: TypedProgram): TypedProgram =
    val (optimized, updated) = constantFoldRecursively(program.syntax, program.types)
    TypedProgram(optimized, updated)

  /** Substitutes constant expressions in `tree` with their results, returning a an updated syntax
    * tree along with a map from each term to its type.
    */
  private def constantFoldRecursively(
      tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): (Syntax[TermTree], TypedProgram.TypeAssignments) = {
    constantFold(tree) match
      case Some(s) =>
        // Constant folding succeeded; return the updated tree.
        (s, Map(s -> types(tree)))

      case _ => tree.value match
        case e: TermTree.TermApplication =>
          // Apply the optimization recursively.
          val (f, ts) = constantFoldRecursively(e.abstraction, types)
          val (a, us) = constantFoldRecursively(e.argument, types)
          val updated = Syntax(TermTree.TermApplication(f, a), tree.span)

          // Fold the result if possible.
          constantFold(updated) match
            case Some(s) => (s, Map(s -> types(tree)))
            case _ => (updated, (ts ++ us).updated(updated, types(tree)))

        case _ =>
          (tree, Map(tree -> types(tree)))
  }

  /** Returns a literal denoting the result of `tree` iff it represents a constant expression. */
  private def constantFold(tree: Syntax[TermTree]): Option[Syntax[TermTree]] =
    import TermTree.TermApplication as F
    tree.value match
      case F(Syntax(F(InfixOperator(f), IntegerConstant(lhs)), _), IntegerConstant(rhs)) =>
        val n = f match
          case InfixOperator.Add => lhs + rhs
          case InfixOperator.Sub => lhs - rhs
        Some(Syntax(TermTree.IntegerLiteral(n), tree.span))
      case _ => None

  private def normalize(tree: Syntax[TermTree], types: TypedProgram.TypeAssignments): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] = 
    import TermTree.Binding as B // let x = 2
    import TermTree.TermApplication as F  // 2
    val originalType = types(tree)

    tree match
      case F(f, Syntax(B(name, initializer, body), _)) =>
        val newTermApplication = Syntax(F(f, body), tree.span)
        val newTree = Syntax(B(name, initializer, newTermApplication), tree.span)
        val allTree = types.updated(newTermApplication, originalType).updated(newTree, originalType)
        Some((newTree, allTree))

      case F(Syntax(B(name, initializer, body), _), arguments) =>
        val newTermApplication = Syntax(F(body, arguments), tree.span)
        val newTree = Syntax(B(name, initializer, newTermApplication), tree.span)
        val allTree = types.updated(newTermApplication, originalType).updated(newTree, originalType)
        Some((newTree, allTree))

      

end Optimizer

/** A pattern for recognizing integer constants. */
private object IntegerConstant:

  def unapply(s: Syntax[TermTree]): Option[Int] =
    s match
      case Syntax(TermTree.IntegerLiteral(n), _) => Some(n)
      case _ => None

end IntegerConstant
