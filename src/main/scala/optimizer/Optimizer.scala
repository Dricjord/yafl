package yafl.optimizer

import yafl.syntax.{InfixOperator, Syntax, TermTree}
import yafl.typer.{Type, TypedProgram}

object Optimizer:

  /** Returns `program` optimized. */
  def optimize(program: TypedProgram): TypedProgram =
    // Fonction interne récursive pour boucler jusqu'à ce que l'arbre se stabilise (Point Fixe)
    def loop(currentSyntax: Syntax[TermTree], currentTypes: TypedProgram.TypeAssignments): (Syntax[TermTree], TypedProgram.TypeAssignments) =
      // 1. Constant folding
      val (folded, foldedTypes) = constantFoldRecursively(currentSyntax, currentTypes)
      // 2. Normalization
      val (normalized, normalizedTypes) = normalizeRecursively(folded, foldedTypes)
      // 3. Constant Propagation (Nouvelle passe)
      val (propagated, propagatedTypes) = propagateConstantsRecursively(normalized, normalizedTypes)
      // 4. Dead Code Elimination
      val (dce, dceTypes) = eliminateDeadCodeRecursively(propagated, propagatedTypes)

      // Si l'AST a changé au cours de ce cycle, on refait un tour d'optimisation
      if dce.value != currentSyntax.value then
        loop(dce, dceTypes)
      else
        (dce, dceTypes)

    val (finalSyntax, finalTypes) = loop(program.syntax, program.types)
    TypedProgram(finalSyntax, finalTypes)

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

  /** Returns a literal denoting the result of tree if it represents a constant expression. */
  private def constantFold(tree: Syntax[TermTree]): Option[Syntax[TermTree]] =
    import TermTree.TermApplication as F
    tree.value match
      case F(Syntax(F(InfixOperator(f), IntegerConstant(lhs)), _), IntegerConstant(rhs)) =>
        f match
          case InfixOperator.Add => Some(Syntax(TermTree.IntegerLiteral(lhs + rhs), tree.span))
          case InfixOperator.Sub => Some(Syntax(TermTree.IntegerLiteral(lhs - rhs), tree.span))
          case InfixOperator.Mul => Some(Syntax(TermTree.IntegerLiteral(lhs * rhs), tree.span))
          case InfixOperator.Div => Some(Syntax(TermTree.IntegerLiteral(lhs / rhs), tree.span))
          case InfixOperator.Eq  => Some(Syntax(TermTree.BooleanLiteral(lhs == rhs), tree.span))
          case InfixOperator.Neq => Some(Syntax(TermTree.BooleanLiteral(lhs != rhs), tree.span))
          case InfixOperator.Lt  => Some(Syntax(TermTree.BooleanLiteral(lhs < rhs), tree.span))
          case InfixOperator.Lte => Some(Syntax(TermTree.BooleanLiteral(lhs <= rhs), tree.span))
          case InfixOperator.Gt  => Some(Syntax(TermTree.BooleanLiteral(lhs > rhs), tree.span))
          case InfixOperator.Gte => Some(Syntax(TermTree.BooleanLiteral(lhs >= rhs), tree.span))
          case _ => None
      case F(Syntax(F(InfixOperator(f), Syntax(TermTree.BooleanLiteral(lhs), _)), _), Syntax(TermTree.BooleanLiteral(rhs), _)) =>
        f match
          case InfixOperator.And => Some(Syntax(TermTree.BooleanLiteral(lhs && rhs), tree.span))
          case InfixOperator.Or  => Some(Syntax(TermTree.BooleanLiteral(lhs || rhs), tree.span))
          case _ => None
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
  
  /** Applique l'élimination du code mort de manière récursive sur tout l'arbre syntaxique. */
  private def eliminateDeadCodeRecursively(tree: Syntax[TermTree], types: TypedProgram.TypeAssignments): (Syntax[TermTree], TypedProgram.TypeAssignments) =
    // On nettoie d'abord les enfants récursivement pour obtenir leurs arbres et leurs types à jour
    val (cleanedTree, cleanedTypes) = tree.value match
      case e: TermTree.Conditional =>
        val (c, ts) = eliminateDeadCodeRecursively(e.condition, types)
        val (s, us) = eliminateDeadCodeRecursively(e.success, types)
        val (f, vs) = eliminateDeadCodeRecursively(e.failure, types)
        val rebuilt = Syntax(TermTree.Conditional(c, s, f), tree.span)
        val currentType = types.getOrElse(tree, types.getOrElse(s, types.getOrElse(f, Type.Ground.Int)))
        (rebuilt, (types ++ ts ++ us ++ vs).updated(rebuilt, currentType))

      case e: TermTree.Binding =>
        val (init, ts) = eliminateDeadCodeRecursively(e.initializer, types)
        val (body, us) = eliminateDeadCodeRecursively(e.body, types)
        val rebuilt = Syntax(TermTree.Binding(e.name, init, body), tree.span)
        (rebuilt, (types ++ ts ++ us).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case e: TermTree.TermApplication =>
        val (abs, ts) = eliminateDeadCodeRecursively(e.abstraction, types)
        val (arg, us) = eliminateDeadCodeRecursively(e.argument, types)
        val rebuilt = Syntax(TermTree.TermApplication(abs, arg), tree.span)
        (rebuilt, (types ++ ts ++ us).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case e: TermTree.TermAbstraction =>
        val (body, ts) = eliminateDeadCodeRecursively(e.body, types)
        val rebuilt = Syntax(TermTree.TermAbstraction(e.parameter, e.ascription, body), tree.span)
        (rebuilt, (types ++ ts).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case e: TermTree.TypeAbstraction =>
        val (body, ts) = eliminateDeadCodeRecursively(e.body, types)
        val rebuilt = Syntax(TermTree.TypeAbstraction(e.parameter, body), tree.span)
        (rebuilt, (types ++ ts).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case e: TermTree.TypeApplication =>
        val (abs, ts) = eliminateDeadCodeRecursively(e.abstraction, types)
        val rebuilt = Syntax(TermTree.TypeApplication(abs, e.argument), tree.span)
        (rebuilt, (types ++ ts).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case e: TermTree.RecursiveAbstraction =>
        val (definition, ts) = eliminateDeadCodeRecursively(e.definition, types)
        val rebuilt = Syntax(TermTree.RecursiveAbstraction(e.name, e.ascription, definition), tree.span)
        (rebuilt, (types ++ ts).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case TermTree.BooleanLiteral(_) =>
        (tree, types.updated(tree, Type.Ground.Bool))

      case TermTree.IntegerLiteral(_) =>
        (tree, types.updated(tree, Type.Ground.Int))

      case _ =>
        val calculatedType = types.getOrElse(tree, Type.Ground.Int)
        (tree, types.updated(tree, calculatedType))

    // Une fois les enfants nettoyés, on applique la règle d'élimination locale au sommet
    eliminateDeadCode(cleanedTree, cleanedTypes) match
      case Some((simplifiedTree, simplifiedTypes)) =>
        // Si le nœud a été simplifié (ex: Conditional -> success), on réapplique l'optimisation sur ce résultat
        eliminateDeadCodeRecursively(simplifiedTree, simplifiedTypes)
      case None =>
        (cleanedTree, cleanedTypes)

  /** Élimine un nœud conditionnel constant ou une liaison non utilisée au sommet de l'arbre. */
  private def eliminateDeadCode(tree: Syntax[TermTree], types: TypedProgram.TypeAssignments): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] =
    tree.value match
      // Conditionnel dont la condition est 'true' -> on extrait la branche 'then'
      case TermTree.Conditional(Syntax(TermTree.BooleanLiteral(true), _), success, _) =>
        val t = types.getOrElse(success, Type.Ground.Int)
        Some((success, types.updated(success, t)))

      // Conditionnel dont la condition est 'false' -> on extrait la branche 'else'
      case TermTree.Conditional(Syntax(TermTree.BooleanLiteral(false), _), _, failure) =>
        val t = types.getOrElse(failure, Type.Ground.Int)
        Some((failure, types.updated(failure, t)))

      // Liaison locale 'let' où la variable n'apparaît jamais dans le corps
      case TermTree.Binding(nameNode, _, body) if !containsVariable(body, nameNode.value.name) =>
        val t = types.getOrElse(body, Type.Ground.Int)
        Some((body, types.updated(body, t)))

      case _ => None

  /** Analyse de manière purement fonctionnelle si une variable est mentionnée librement dans un arbre. */
  private def containsVariable(tree: Syntax[TermTree], name: String): Boolean = tree.value match
    case TermTree.Variable(n) =>
      n == name
    case TermTree.TermAbstraction(param, _, body) =>
      if param.value.name == name then false else containsVariable(body, name)
    case TermTree.TermApplication(abstraction, argument) =>
      containsVariable(abstraction, name) || containsVariable(argument, name)
    case TermTree.TypeAbstraction(_, body) =>
      containsVariable(body, name)
    case TermTree.TypeApplication(abstraction, _) =>
      containsVariable(abstraction, name)
    case TermTree.Conditional(condition, success, failure) =>
      containsVariable(condition, name) || containsVariable(success, name) || containsVariable(failure, name)
    case TermTree.Binding(bindingName, initializer, body) =>
      containsVariable(initializer, name) || (if bindingName.value.name == name then false else containsVariable(body, name))
    case TermTree.RecursiveAbstraction(bindingName, _, definition) =>
      if bindingName.value.name == name then false else containsVariable(definition, name)
    case _ =>
      false

  
  /** Remplace récursivement toutes les occurrences libres de la variable `name` par l'expression `replacement`. */
  private def substitute(tree: Syntax[TermTree], name: String, replacement: Syntax[TermTree]): Syntax[TermTree] =
    tree.value match
      case TermTree.Variable(n) =>
        if n == name then replacement else tree

      case TermTree.TermAbstraction(param, ascription, body) =>
        // Si le paramètre de la fonction masque notre variable, on ne descend pas plus bas
        if param.value.name == name then tree
        else Syntax(TermTree.TermAbstraction(param, ascription, substitute(body, name, replacement)), tree.span)

      case TermTree.TermApplication(abstraction, argument) =>
        Syntax(TermTree.TermApplication(substitute(abstraction, name, replacement), substitute(argument, name, replacement)), tree.span)

      case TermTree.TypeAbstraction(parameter, body) =>
        Syntax(TermTree.TypeAbstraction(parameter, substitute(body, name, replacement)), tree.span)

      case TermTree.TypeApplication(abstraction, argument) =>
        Syntax(TermTree.TypeApplication(substitute(abstraction, name, replacement), argument), tree.span)

      case TermTree.Conditional(condition, success, failure) =>
        Syntax(TermTree.Conditional(substitute(condition, name, replacement), substitute(success, name, replacement), substitute(failure, name, replacement)), tree.span)

      case TermTree.Binding(bindingName, initializer, body) =>
        val nextInit = substitute(initializer, name, replacement)
        val nextBody = if bindingName.value.name == name then body else substitute(body, name, replacement)
        Syntax(TermTree.Binding(bindingName, nextInit, nextBody), tree.span)

      case TermTree.RecursiveAbstraction(bindingName, ascription, definition) =>
        val nextDef = if bindingName.value.name == name then definition else substitute(definition, name, replacement)
        Syntax(TermTree.RecursiveAbstraction(bindingName, ascription, nextDef), tree.span)

      case _ =>
        tree
        
  /** Applique la propagation de constantes de manière récursive sur tout l'arbre syntaxique. */
  private def propagateConstantsRecursively(
      tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): (Syntax[TermTree], TypedProgram.TypeAssignments) =
    val (cleanedTree, cleanedTypes) = tree.value match
      case e: TermTree.Binding =>
        // On optimise d'abord l'initialiseur et le corps
        val (init, ts) = propagateConstantsRecursively(e.initializer, types)
        val (body, us) = propagateConstantsRecursively(e.body, types)
        
        init.value match
          // Si l'initialiseur est une constante prouvée (Int ou Bool), on la propage dans le corps !
          case TermTree.IntegerLiteral(_) | TermTree.BooleanLiteral(_) =>
            val substitutedBody = substitute(body, e.name.value.name, init)
            // On recrée temporairement le binding avec le corps substitué
            val rebuilt = Syntax(TermTree.Binding(e.name, init, substitutedBody), tree.span)
            (rebuilt, (types ++ ts ++ us).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))
          case _ =>
            val rebuilt = Syntax(TermTree.Binding(e.name, init, body), tree.span)
            (rebuilt, (types ++ ts ++ us).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case e: TermTree.Conditional =>
        val (c, ts) = propagateConstantsRecursively(e.condition, types)
        val (s, us) = propagateConstantsRecursively(e.success, types)
        val (f, vs) = propagateConstantsRecursively(e.failure, types)
        val rebuilt = Syntax(TermTree.Conditional(c, s, f), tree.span)
        (rebuilt, (types ++ ts ++ us ++ vs).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case e: TermTree.TermApplication =>
        val (abs, ts) = propagateConstantsRecursively(e.abstraction, types)
        val (arg, us) = propagateConstantsRecursively(e.argument, types)
        val rebuilt = Syntax(TermTree.TermApplication(abs, arg), tree.span)
        (rebuilt, (types ++ ts ++ us).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case e: TermTree.TermAbstraction =>
        val (body, ts) = propagateConstantsRecursively(e.body, types)
        val rebuilt = Syntax(TermTree.TermAbstraction(e.parameter, e.ascription, body), tree.span)
        (rebuilt, (types ++ ts).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case e: TermTree.TypeAbstraction =>
        val (body, ts) = propagateConstantsRecursively(e.body, types)
        val rebuilt = Syntax(TermTree.TypeAbstraction(e.parameter, body), tree.span)
        (rebuilt, (types ++ ts).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case e: TermTree.TypeApplication =>
        val (abs, ts) = propagateConstantsRecursively(e.abstraction, types)
        val rebuilt = Syntax(TermTree.TypeApplication(abs, e.argument), tree.span)
        (rebuilt, (types ++ ts).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case e: TermTree.RecursiveAbstraction =>
        val (definition, ts) = propagateConstantsRecursively(e.definition, types)
        val rebuilt = Syntax(TermTree.RecursiveAbstraction(e.name, e.ascription, definition), tree.span)
        (rebuilt, (types ++ ts).updated(rebuilt, types.getOrElse(tree, Type.Ground.Int)))

      case _ =>
        (tree, types.updated(tree, types.getOrElse(tree, Type.Ground.Int)))

    // Une passe optionnelle pour mettre à jour la table des types des expressions substituées
    // (Puisque les nœuds substitués prennent le type de la constante propagée)
    val finalTypes = cleanedTree.value match
      case TermTree.Binding(_, init, body) =>
        val initType = cleanedTypes.getOrElse(init, Type.Ground.Int)
        // On s'assure que la map des types contienne une entrée valide pour le corps restructuré
        cleanedTypes.updated(body, cleanedTypes.getOrElse(body, initType))
      case _ => cleanedTypes

    (cleanedTree, finalTypes)
end Optimizer

/** A pattern for recognizing integer constants. */
private object IntegerConstant:

  def unapply(s: Syntax[TermTree]): Option[Int] =
    s match
      case Syntax(TermTree.IntegerLiteral(n), _) => Some(n)
      case _ => None

end IntegerConstant
