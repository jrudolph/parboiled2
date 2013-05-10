package org.parboiled
package optree

class OpTreeContext(c: Parser.ParserContext) {
  import c.universe._

  abstract class Expression {
    def render: c.Expr[Boolean] = ???
  }

  case class LiteralChar(val c: Char) extends Expression

  case class LiteralString(val c: String) extends Expression

  case object EOI extends Expression {
    def unapply(tree: c.Tree): Option[EOI.type] = tree match {
      case Apply(Select(This(typeName), termName), List(Select(This(argTypeName), argTermName))) if argTermName.decoded == "EOI" ⇒
        Some(EOI)
      case _ ⇒ None
    }
  }

  case class CharacterClass(val chars: Array[Char]) extends Expression

  case class AnyCharacter() extends Expression

  case class Grouping(val n: Expression) extends Expression

  case class Optional(val n: Expression) extends Expression

  case class ZeroOrOne(val n: Expression) extends Expression

  case class OneOrMore(val n: Expression) extends Expression

  case class AndPredicate(val n: Expression) extends Expression

  case class NotPredicate(val n: Expression) extends Expression

  case class Sequence(val lhs: Expression, val rhs: Expression) extends Expression

  case class FirstOf(val lhs: Expression, val rhs: Expression) extends Expression

  object LiteralString {
    def unapply(tree: c.Tree): Option[LiteralString] = tree match {
      case Apply(Select(This(typeName), termName), List(Literal(Constant(x: String)))) ⇒ Some(LiteralString(x))
      case _ ⇒ None
    }
  }

  object OpTree {
    def unapply(tree: c.Tree): Option[Expression] = tree match {
      case LiteralString(x) ⇒ Some(x)
      case _                ⇒ ???
    }
  }
}