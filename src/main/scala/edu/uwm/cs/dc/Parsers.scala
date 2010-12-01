package edu.uwm.cs.dc

import scala.collection.mutable

trait Parsers {
  val epsilon: Parser = new Parser {
    val isNullable = true
    
    def derive(c: Char) = empty
    
    override def toString = "<>"
  }
  
  private val empty = new Parser {
    val isNullable = false
    
    def derive(c: Char) = error("Cannot derive the empty parser")
    
    override def ~(that: =>Parser) = this
    
    override def toString = "Ø"
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
    
    override def toString = "<union>"
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
    
    override def toString = left.toString + " ~ " + right.toString
  }
  
  case class LiteralParser(c: Char) extends Parser {
    val isNullable = false
    
    def derive(c: Char) =
      if (this.c == c) epsilon else empty
    
    override def toString = c.toString
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
