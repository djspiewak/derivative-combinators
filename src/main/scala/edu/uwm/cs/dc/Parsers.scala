package edu.uwm.cs.dc

import scala.collection.mutable

trait Parsers {
  val epsilon: Parser = new Parser {
    val isNullable = true
    
    def derive(c: Char) = None
  }
  
  implicit def literal(c: Char): Parser = LiteralParser(c)
  
  implicit def unionSyntax(c: Char): RichParser = unionSyntax(literal(c))
  
  implicit def unionSyntax(left: =>Parser): RichParser = new RichParser(left)
  
  
  class RichParser(left: =>Parser) {
    def |(right: =>Parser): Parser = new UnionParser(left, right)
  }
  
  
  trait Parser extends (Stream[Char] => Boolean) {
    def isNullable: Boolean
    
    def derive(c: Char): Option[Parser]
    
    def apply(str: Stream[Char]) = str match {
      case hd #:: tail => 
        derive(hd) map { _(tail) } getOrElse false
      
      case Stream() => isNullable
    }
    
    def ~(that: =>Parser): Parser = new ConcatParser(this, that)
  }
  
  class UnionParser(_left: =>Parser, _right: =>Parser) extends Parser with MemoizedDerivation {
    lazy val left = _left
    lazy val right = _right
    
    lazy val isNullable = left.isNullable || right.isNullable
    
    def innerDerive(c: Char) = {
      val leftPotential = left derive c
      val rightPotential = right derive c
      
      val combinedPotential = for {
        ld <- leftPotential
        rd <- rightPotential
      } yield ld | rd
      
      combinedPotential orElse leftPotential orElse rightPotential
    }
  }
  
  class ConcatParser(_left: =>Parser, _right: =>Parser) extends Parser with MemoizedDerivation {
    lazy val left = _left
    lazy val right = _right
    
    lazy val isNullable = !left.isNullable || right.isNullable
    lazy val isNullable = left.isNullable && right.isNullable
    
    def innerDerive(c: Char) = {
      val leftPotential = left derive c
      lazy val rightPotential = right derive c
      
      val concatPotential = leftPotential map { _ ~ right }
      
      if (left.isNullable) {
        val combinedPotential = for {
          lc <- concatPotential
          rd <- rightPotential
        } yield lc | rd
        
        combinedPotential orElse concatPotential orElse rightPotential
      } else {
        concatPotential
      }
    }
  }
  
  case class LiteralParser(c: Char) extends Parser {
    val isNullable = false
    
    def derive(c: Char) =
      if (this.c == c) Some(epsilon) else None
  }
  
  trait MemoizedDerivation extends Parser {
    private val derivations = mutable.Map[Char, Option[Parser]]()
    
    final def derive(c: Char) = derivations get c getOrElse {
      derivations += (c -> None)
      
      val back = innerDerive(c)
      derivations(c) = back
      
      back
    }
    
    protected def innerDerive(c: Char): Option[Parser]
  }
}
