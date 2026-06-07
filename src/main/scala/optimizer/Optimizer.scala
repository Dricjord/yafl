package yafl.optimizer

import yafl.syntax.{InfixOperator, Syntax, TermTree}
import yafl.typer.{Type, TypedProgram}

object Optimizer:

  /** Returns `program` optimized. */
  def optimize(program: TypedProgram): TypedProgram =
    val (folded, foldedTypes) = constantFoldRecursively(program.syntax, program.types)
    val (normalized, normalizedTypes) = normalizeRecursively(folded, foldedTypes)
    TypedProgram(normalized, normalizedTypes)

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

        case e: TermTree.Binding =>
          val (init, ts) = constantFoldRecursively(e.initializer, types)
          val (body, us) = constantFoldRecursively(e.body, types)
          val updated = Syntax(TermTree.Binding(e.name, init, body), tree.span)
          (updated, (ts ++ us).updated(updated, types(tree)))

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

  private def normalizeRecursively(
      tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): (Syntax[TermTree], TypedProgram.TypeAssignments) =
    normalize(tree, types) match
      case Some((normalizedTree, normalizedTypes)) =>
        normalizeRecursively(normalizedTree, normalizedTypes)
      case None =>
        tree.value match
          case e: TermTree.TermApplication =>
            val (abstraction, abstractionTypes) = normalizeRecursively(e.abstraction, types)
            val (argument, argumentTypes) = normalizeRecursively(e.argument, types)
            val rebuilt = Syntax(TermTree.TermApplication(abstraction, argument), tree.span)
            val rebuiltTypes = (abstractionTypes ++ argumentTypes).updated(rebuilt, types(tree))
            normalize(rebuilt, rebuiltTypes) match
              case Some((normalizedTree, normalizedTypes)) => normalizeRecursively(normalizedTree, normalizedTypes)
              case None => (rebuilt, rebuiltTypes)
          case e: TermTree.Binding =>
            val (initializer, initTypes) = normalizeRecursively(e.initializer, types)
            val (body, bodyTypes) = normalizeRecursively(e.body, types)
            val rebuilt = Syntax(TermTree.Binding(e.name, initializer, body), tree.span)
            (rebuilt, (initTypes ++ bodyTypes).updated(rebuilt, types(tree)))
          case _ =>
            (tree, Map(tree -> types(tree)))

  private def normalize(tree: Syntax[TermTree], types: TypedProgram.TypeAssignments): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] =
    import TermTree.Binding as B
    import TermTree.TermApplication as F
    val originalType = types(tree)

    tree.value match
      // f (let x = init; body)  =>  let x = init; f body
      case F(abstraction, Syntax(B(name, initializer, body), _)) =>
        val floatedApp = Syntax(F(abstraction, body), tree.span)
        val floatedBinding = Syntax(B(name, initializer, floatedApp), tree.span)
        val updatedTypes = types.updated(floatedApp, originalType).updated(floatedBinding, originalType)
        Some((floatedBinding, updatedTypes))

      // (let x = init; body) arg  =>  let x = init; body arg
      case F(Syntax(B(name, initializer, body), _), argument) =>
        val floatedApp = Syntax(F(body, argument), tree.span)
        val floatedBinding = Syntax(B(name, initializer, floatedApp), tree.span)
        val updatedTypes = types.updated(floatedApp, originalType).updated(floatedBinding, originalType)
        Some((floatedBinding, updatedTypes))

      // ((op + c1) variable) + c2  =>  (op + (c1 + c2)) variable  (reassociate constants left)
      case F(outerApp @ Syntax(F(op @ InfixOperator(outerOp), Syntax(F(Syntax(F(InfixOperator(innerOp), IntegerConstant(leftConst)), _), variable), _)), _),
        IntegerConstant(rightConst)) if outerOp == InfixOperator.Add && innerOp == InfixOperator.Add && IntegerConstant.unapply(variable).isEmpty =>
        val mergedConst    = Syntax(TermTree.IntegerLiteral(leftConst + rightConst), tree.span)
        val reassociatedOp = Syntax(F(op, mergedConst), outerApp.span)
        val reassociatedTree = Syntax(F(reassociatedOp, variable), tree.span)
        Some((reassociatedTree, types
          .updated(mergedConst,      Type.Ground.Int)
          .updated(reassociatedOp,   types(outerApp))
          .updated(reassociatedTree, originalType)))

      // (op variable) + constant  =>  (op constant) variable  (move variable to the right)
      case F(partialApp @ Syntax(F(op @ InfixOperator(operator), variable), _), constant @ IntegerConstant(_))
          if operator == InfixOperator.Add && IntegerConstant.unapply(variable).isEmpty =>
        val swappedOp   = Syntax(F(op, constant), partialApp.span)
        val swappedTree = Syntax(F(swappedOp, variable), tree.span)
        Some((swappedTree, types
          .updated(swappedOp,   types(partialApp))
          .updated(swappedTree, originalType)))

      case _ => None
      

end Optimizer

/** A pattern for recognizing integer constants. */
private object IntegerConstant:

  def unapply(s: Syntax[TermTree]): Option[Int] =
    s match
      case Syntax(TermTree.IntegerLiteral(n), _) => Some(n)
      case _ => None

end IntegerConstant
