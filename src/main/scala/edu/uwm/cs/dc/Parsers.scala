package edu.uwm.cs.dc

import scala.collection.mutable

trait Parsers {
  val epsilon: Parser = new Parser {
    val isNullable = true
    
    def derive(c: Char) = empty
    
    def innerToString(visited: Set[Parser]) = ("<>", visited)
  }
  
  private val empty = new Parser {
    val isNullable = false
    
    def derive(c: Char) = error("Cannot derive the empty parser")
    
    override def ~(that: =>Parser) = this
    
    def innerToString(visited: Set[Parser]) = ("{}", visited)
  }
  
  implicit def literal(c: Char): Parser = LiteralParser(c)
  
  implicit def unionSyntax(c: Char): RichParser = unionSyntax(literal(c))
  
  implicit def unionSyntax(left: =>Parser): RichParser = new RichParser(left)
  
  
  class RichParser(left: =>Parser) {
    def |(right: =>Parser): Parser = new UnionParser(left, right)
  }
  
  
  trait Parser extends (Stream[Char] => Boolean) {
    def isNullable: Boolean
    
    def derive(c: Char): Parser
    
    def apply(str: Stream[Char]) = str match {
      case hd #:: tail => derive(hd)(tail)
      
      case Stream() => isNullable
    }
    
    def ~(that: =>Parser): Parser = new ConcatParser(this, that)
    
    override def toString = innerToString(Set())._1
    
    def innerToString(visited: Set[Parser]): (String, Set[Parser])
  }
  
  class UnionParser(_left: =>Parser, _right: =>Parser) extends Parser with MemoizedDerivation {
    lazy val left = _left
    lazy val right = _right
    
    private var _isNullable: Option[Boolean] = None
    
    def isNullable = _isNullable getOrElse {
      _isNullable = Some(false)
      var back = left.isNullable || right.isNullable
      _isNullable = Some(back)
      
      // try to achieve a fixpoint
      while ((left.isNullable || right.isNullable) != back) {
        back = left.isNullable || right.isNullable
        _isNullable = Some(back)
      }
      
      back
    }
    
    def innerDerive(c: Char) = {
      if (left == empty && right == empty)
        empty
      else if (left == empty)
        right.derive(c)
      else if (right == empty)
        left.derive(c)
      else
        left.derive(c) | right.derive(c)
    }
    
    def innerToString(visited: Set[Parser]) = {
      if (visited contains this)
        (hashCode.toString, visited)
      else {
        val (leftStr, leftVisit) = left.innerToString(visited + this)
        val (rightStr, rightVisit) = right.innerToString(leftVisit)
        ("(| " + leftStr + " " + rightStr + ")", rightVisit)
      }
    }
  }
  
  class ConcatParser(_left: =>Parser, _right: =>Parser) extends Parser {
    lazy val left = _left
    lazy val right = _right
    
    def isNullable = left.isNullable && right.isNullable
    
    def derive(c: Char) = {
      if (left.isNullable)
        left.derive(c) ~ right | right.derive(c)
      else
        left.derive(c) ~ right
    }
    
    def innerToString(visited: Set[Parser]) = {
      val (leftStr, leftVisited) = left.innerToString(visited)
      val (rightStr, rightVisited) = right.innerToString(visited)
      ("(~ " + leftStr + " " + rightStr + ")", rightVisited)
    }
  }
  
  case class LiteralParser(c: Char) extends Parser {
    val isNullable = false
    
    def derive(c: Char) =
      if (this.c == c) epsilon else empty
    
    def innerToString(visited: Set[Parser]) = (c.toString, visited)
  }
  
  trait MemoizedDerivation extends Parser {
    private val derivations = mutable.Map[Char, Parser]()
    
    final def derive(c: Char) = derivations get c getOrElse {
      val back = innerDerive(c)
      derivations(c) = back
      
      back
    }
    
    protected def innerDerive(c: Char): Parser
  }
}
