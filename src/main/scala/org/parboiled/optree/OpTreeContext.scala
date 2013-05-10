package org.parboiled
package optree

abstract class OpTree {
  def render(c: Parser.ParserContext): c.Expr[Boolean]
}

object OpTree {

  // format: OFF
  def apply(c: Parser.ParserContext)(tree: c.Tree): OpTree = (
    EOI(c)(tree) orElse
    LiteralString(c)(tree) // orElse ...
    getOrElse c.abort(c.enclosingPosition, "Invalid rule definition")
  )
  // format: ON
}

case object EOI extends OpTree {
  def render(c: Parser.ParserContext): c.Expr[Boolean] = ???

  def apply(c: Parser.ParserContext)(tree: c.Tree): Option[EOI.type] = {
    import c.universe._
    tree match {
      case Apply(Select(This(typeName), termName), List(Select(This(argTypeName), argTermName))) if argTermName.decoded == "EOI" ⇒
        Some(EOI)
      case _ ⇒ None
    }
  }
}

case class LiteralString(c: String) extends OpTree {
  def render(c: Parser.ParserContext): c.Expr[Boolean] = ???
}

object LiteralString {
  def apply(c: Parser.ParserContext)(tree: c.Tree): Option[LiteralString] = {
    import c.universe._
    tree match {
      case Apply(Select(This(typeName), termName), List(Literal(Constant(x: String)))) ⇒ Some(LiteralString(x))
      case _ ⇒ None
    }
  }
}

//  case class LiteralChar(c: Char) extends OpTree
//
//  case class CharacterClass(chars: Array[Char]) extends OpTree
//
//  case class AnyCharacter() extends OpTree
//
//  case class Grouping(n: OpTree) extends OpTree
//
//  case class Optional(n: OpTree) extends OpTree
//
//  case class ZeroOrOne(n: OpTree) extends OpTree
//
//  case class OneOrMore(n: OpTree) extends OpTree
//
//  case class AndPredicate(n: OpTree) extends OpTree
//
//  case class NotPredicate(n: OpTree) extends OpTree
//
//  case class Sequence(lhs: OpTree, rhs: OpTree) extends OpTree
//
//  case class FirstOf(lhs: OpTree, rhs: OpTree) extends OpTree