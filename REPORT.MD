# Introduction

Repository link https://github.com/Dricjord/yafl

Tag link : https://github.com/Dricjord/yafl/releases/tag/v1.0

# Persons

- Luan Cortes Zuka (Luan-CortesZ)
- Filipe Da Silva (Dricjord)
- Anthony Zufferey (zcZufAnt)

# Required Tasks

## Conditionals and Bindings

_Persons_: **Filipe**, **Luan**, **Anthony**

_Implementation_ :
Le support des conditionnelles (`if/then/else`) et des liaisons locales (`let x = e ; body`) a été complété à travers les composants suivants :

- **[Parser.scala (conditional)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L207-L221)**
- **[Parser.scala (binding)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L224-L234)**
- **[Typer.scala (Conditional)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/typer/Typer.scala#L131-L140)**
- **[Typer.scala (Binding)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/typer/Typer.scala#L142-L145)**

## Type abstractions

_Persons_: **Filipe**

_Implementation_ :
Le support des abstractions de types (`[T] => e`) a été complété à travers les composants suivants :

- **[Parser.scala (typeAbstraction)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L243-L259)**
- **[Typer.scala (TypeAbstraction)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/typer/Typer.scala#L119-L121)**

## Prefix terms

_Persons_: **Filipe** (avec l'aide d'Allan Brunner)

_Implementation_ :
Le support des termes préfixes (`op e`) a été complété à travers les composants suivants :

- **[Parser.scala (prefixTerm)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L83-L97)**

## Universal types

_Persons_: **Filipe** (avec l'aide d'Allan Brunner)

_Implementation_ :
Le support des types universels (`[T] => U`, alias _forall_) a été complété à travers les composants suivants :

- **[Parser.scala (universalType)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L358-L375)**

## Arrow types

_Persons_: **Luan**

_Implementation_ :
Le support des types flèches (`T -> U`), qui représentent le type des fonctions ou abstractions de termes, a été complété à travers les composants suivants :

- **[Parser.scala (typ3)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L325-L338)**

## Parenthesized types

_Persons_: **Luan**

_Implementation_ :
Le support des types entre parenthèses a été complété à travers les composants suivants :

- **[Parser.scala (parenthesizedType)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L379-L389)**

## Type applications

_Persons_: **Anthony**

_Implementation_ :
Construit un noeud TypeApplication(e, T) dans l'AST lorsqu'un terme est suivi d'un type entre crochets `e [T]`. Complété grâce aux fonctions suivantes :

- **[Parser.scala (typeApplication)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L97-L129)**
- **[Parser.scala (trailingTypeArguments)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L310-L338)**

## Recursive abstractions

_Persons_: **Anthony**

_Implementation_ :
Définit un terme autoréférentiel `fix x : T = f` où `x` est accessible dans `f`. Implémenté grâce à la fonction suivante:

- **[Parser.scala (recursiveAbstraction)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L263-L279)**

# Optional Taks

## Multiple parameters and arguments

_Persons_: **Anthony**

_Implementation_ :
Désucre les paramètres multiples en abstractions imbriquées et les arguments de type multiples en applications chaînées.

- **[Parser.scala (trailingTermParameters)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L284-L294)**
- **[Parser.scala (trailingTypeParameters)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L296-L308)**
- **[Parser.scala (trailingTypeArguments)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L310-L338)**

## Normalization

_Persons_: **Filipe**

_Implementation_ :
La normalisation a été implémentée de manière purement fonctionnelle dans l'objet `Optimizer`. Elle déplace les liaisons vers la racine de l'arbre et réassocie les constantes vers la gauche :

- **[Optimizer.scala (normalizeRecursively)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/optimizer/Optimizer.scala#L87-L109)**
- **[Optimizer.scala (normalize)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/optimizer/Optimizer.scala#L111-L151)**

L'utilisation de Claude est documentée directement dans le commentaire de la fonction.

## Dead code elimination

_Persons_: **Luan**

_Implementation_ :
L'élimination du code mort a été implémentée de manière purement fonctionnelle dans l'objet `Optimizer` :

- **[Optimizer.scala (eliminateDeadCode)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/optimizer/Optimizer.scala#L216-L233)**
- **[Optimizer.scala (eliminateDeadCodeRecursively)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/optimizer/Optimizer.scala#L154-L213)**
- **[Optimizer.scala (containsVariable)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/optimizer/Optimizer.scala#L236-L254)**

Les tests unitaires validant le comportement se trouvent dans :

- **[OptimizerTests.scala](https://github.com/Dricjord/yafl/blob/main/src/test/scala/Optimizer.scala#L25-L42)**

L'implémentation ainsi que les tests ont grandement été aidés par l'IA Gemini

## Constant propagation

_Persons_: **Luan**

_Implementation_ :
La propagation de constantes a été implémentée de manière purement fonctionnelle au sein de l'objet `Optimizer`:

- **[Optimizer.scala (propagateConstantsRecursively)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/optimizer/Optimizer.scala#L292-L357)**
- **[Optimizer.scala (substitute)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/optimizer/Optimizer.scala#L258-L290)**

Les tests unitaires validant le comportement combiné de la propagation et du nettoyage se trouvent dans :

- **[OptimizerTests.scala](https://github.com/Dricjord/yafl/blob/main/src/test/scala/Optimizer.scala#L44-L49)**

L'implémentation ainsi que les tests ont grandement été aidés par l'IA Gemini.

## Built-in arithmetic and comparison

_Persons_: **Filipe**

_Implementation_ :
Le support complet des opérations arithmétiques et de comparaison entières a été ajouté dans les composants suivants :

- **[Typer.scala (Environment.builtin)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/typer/Typer.scala#L38-L69)**
- **[Emitter.scala (InfixOperator)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/emitter/Emitter.scala#L109-L125)**

## Bindings

_Persons_: **Filipe**

_Implementation_ :
Le support de la génération de code pour les liaisons locales a été implémenté en utilisant les variables locales WebAssembly :

- **[Emitter.scala (Binding)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/emitter/Emitter.scala#L97-L107)**

Les tests unitaires validant le comportement se trouvent dans :

- **[EmitterTests.scala (binding, binding shadowing)](https://github.com/Dricjord/yafl/blob/main/src/test/scala/Emitter.scala#L33-L47)**
