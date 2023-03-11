package com.github.distcompiler.dcal

/**
 * An low-level IR that resembles an annotated TLA+ string but preserves identifiers and let expressions in structures
 * for further optimization.
 */
object IR {
  /*
  module ::= `MODULE` <name> <definition>*

  definition ::= <name> `(` (<name> (`,` <name>)* )? `)` `==` <node>*

  let ::= `LET` ( <name> `==` <node>+ )+ `IN` <node>+

  node ::=
  | <name>
  | <let>
  | <uninterpreted>
  */
  enum Node {
    // For a Name name to be a valid, there should exist somewhere another node that explains what name is bound to
    case Name(name: String)
    case Let(name: String, binding: List[Node], body: List[Node])
    case Uninterpreted(text: String)
    case MapOnSet(set: List[Node], setMember: String, proc: List[IR.Node])
    case FilterOnSet(set: List[Node], setMember: String, pred: List[IR.Node])
  }

  final case class Definition(name: String, params: List[String], body: List[Node])

  final case class Module(name: String, definitions: List[Definition])
}
