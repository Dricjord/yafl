
# Introduction
Repository link https://github.com/Dricjord/yafl
# Required Tasks

## Conditionals and Bindings

*Persons*: 

*Implementation* :

## Type abstractions

*Persons*: 

*Implementation* :

## Prefix terms

*Persons*: 

*Implementation* :

## Universal types

*Persons*:

*Implementation* : 


## Arrow types

*Persons*: **Luan**

*Implementation* : 
Le support des types flèches (`T -> U`), qui représentent le type des fonctions ou abstractions de termes, a été complété à travers les composants suivants :
* **[Parser.scala (typ3)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L325-L338)** 

## Parenthesized types

*Persons*: **Luan**

*Implementation* : 
Le support des types entre parenthèses a été complété à travers les composants suivants :
* **[Parser.scala (parenthesizedType)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/parser/Parser.scala#L379-L389)**

## Type applications

*Persons*: 

*Implementation* :

## Recursive abstractions

*Persons*: 

*Implementation* :

# Optional Taks

## Multiple parameters and arguments

*Persons*: 

*Implementation* :

## Normalization 

*Persons*: 

*Implementation* :

## Dead code elimination 

*Persons*: **Luan**

*Implementation* : 
L'élimination du code mort a été implémentée de manière purement fonctionnelle dans l'objet `Optimizer` :
* **[Optimizer.scala (eliminateDeadCode)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/optimizer/Optimizer.scala#L216-L233)** 
* **[Optimizer.scala (eliminateDeadCodeRecursively)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/optimizer/Optimizer.scala#L154-L213)** 
* **[Optimizer.scala (containsVariable)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/optimizer/Optimizer.scala#L236-L254)**

Les tests unitaires validant le comportement se trouvent dans :
* **[OptimizerTests.scala](https://github.com/Dricjord/yafl/blob/main/src/test/scala/Optimizer.scala#L25-L42)** 

L'implémentation ainsi que les tests ont grandement été aidés par l'IA Gemini


## Constant propagation

*Persons*: **Luan**

*Implementation* : 
La propagation de constantes a été implémentée de manière purement fonctionnelle au sein de l'objet `Optimizer`:
* **[Optimizer.scala (propagateConstantsRecursively)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/optimizer/Optimizer.scala#L292-L357)** 
* **[Optimizer.scala (substitute)](https://github.com/Dricjord/yafl/blob/main/src/main/scala/optimizer/Optimizer.scala#L258-L290)**

Les tests unitaires validant le comportement combiné de la propagation et du nettoyage se trouvent dans :
* **[OptimizerTests.scala](https://github.com/Dricjord/yafl/blob/main/src/test/scala/Optimizer.scala#L44-L49)** 

L'implémentation ainsi que les tests ont grandement été aidés par l'IA Gemini.

## Built-in arithmetic and comparison

*Persons*: 

*Implementation* :

## Bindings 

*Persons*: 

*Implementation* :

