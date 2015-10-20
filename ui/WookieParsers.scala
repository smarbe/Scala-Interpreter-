package ui

import scala.util.parsing.combinator._
import expressions._
import values._

class WookieParsers extends RegexParsers {

  // EXPRESSION ::= DECLARATION | CONDITIONAL | DISJUNCTION
  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  // DECLARATION ::= def~IDENTIFIER~=~EXPRESSION 
  def declaration: Parser[Expression] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def" ~ x ~ "=" ~ y => Declaration(x, y)
  }

  // CONDITIONAL ::= if~(~EXPRESSION~)~EXPRESSION~(else~EXPRESSION)?
  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if" ~ "(" ~ x ~ ")" ~ y ~ None => Conditional(x, y)
    case "if" ~ "(" ~ x ~ ")" ~ y ~ Some("else" ~ z) => Conditional(x, y, z)
  }

  // DISJUNCTION ::= CONJUNCTION~(||~CONJUNCTION)*
  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil => con
    case con ~ cons => Disjunction(con :: cons)
  }

  // CONJUNCTION ::= EQUALITY ~ (&&~EQUALITY)*
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case eq ~ Nil => eq
    case eq ~ eqs => Conjunction(eq :: eqs)
  }

  // EQUALITY ::= INEQUALITY~(==~INEQUALITY)*
  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
    case ineq ~ Nil => ineq
    case ineq ~ ineqs => FunCall(Identifier("equals"), ineq :: ineqs)
  }

  // INEQUALITY ::= SUM~((<|>)~SUM)*
  def inequality: Parser[Expression] = sum ~ rep("<" ~> sum) ^^ {
    case sum ~ Nil => sum
    case sum ~ sums => FunCall(Identifier("less"), sum :: sums)
  }

  // SUM ::= PRODUCT~((\+|-)~PRODUCT)*
  def sum: Parser[Expression] = product ~ rep(("+" | "-") ~ product ^^ { case "+" ~ s => s case "-" ~ s => negate(s) }) ^^ {
    case p ~ Nil => p
    case p ~ rest => FunCall(Identifier("add"), p :: rest)
  }

  // PRODUCT ::= FUNCALL~((\*|/)~FUNCALL)*
  def product: Parser[Expression] = funcall ~ rep(("*" | "/") ~ funcall ^^ { case "*" ~ s => s case "/" ~ s => invert(s) }) ^^ {
    case p ~ Nil => p
    case p ~ rest => FunCall(Identifier("mul"), p :: rest)
  }

  //--------------------------------------------------------------
  // FUNCALL ::= TERM~OPERANDS?
  def funcall: Parser[Expression] = term ~ opt(operands) ^^ {
    case ter ~ None => ter
    case ter ~ Some(ops) => FunCall(ter.asInstanceOf[Expression], ops)
  }

  // OPERANDS ::= (~(EXPRESSION~(,~EXPRESSION)*)?)
  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^ {
    case None => Nil
    case Some(expr ~ Nil) => List(expr)
    case Some(expr ~ exprs) => expr :: exprs
  }

  //--------------------------------------------------------------
  // TERM ::= LITERAL | IDENTIFIER  | (~EXPRESSION~)
  def term: Parser[Expression] = lambda | block | literal | identifier | "(" ~> expression <~ ")"

  // LITERAL ::= BOOLE | NUMBER
  def literal: Parser[Literal] = boole | number

  // IDENTIFIER ::= [a-zA-Z][0-9a-zA-Z]*
  def identifier: Parser[Identifier] = """[a-zA-Z]([0-9a-zA-Z])*""".r ^^ {
    case x => Identifier(x)
  }

  // BOOLE ::= true | false
  def boole: Parser[Boole] = ("false" | "true") ^^ {
    case boole => new Boole(boole.toBoolean)
  }

  // NUMBER ::= (\+|-)?[0-9]+(\.[0-9]+)?
  def number: Parser[Number] = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^ {
    case num => new Number(num.toDouble)
  }

  //-----------------------------------------------------------------  
  // LAMBDA ::= lambda PARAMETERS EXPRESSION
  def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^ {
    case "lambda" ~ param ~ expr => Lambda(param, expr)
  }
  
  // PARAMETERS ::= ((IDENTIFIER (, IDENTIFIER)*))?)
  def parameters: Parser[List[Identifier]] = "(" ~ opt(identifier ~ rep(identifier)) ~ ")" ^^ {
    case "(" ~ None ~ ")" => Nil
    case "(" ~ Some(ident ~ Nil) ~ ")" => List(ident)
    case "(" ~ Some(ident ~ idents) ~ ")" => ident :: idents
    
  }
  
  // BLOCK ::- {EXPRESSION (; EXPRESSION)*}
  def block: Parser[Expression] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ expr ~ Nil ~ "}" => Block(List(expr))
    case "{" ~ expr ~ exprs ~ "}" => Block(expr :: exprs)
  }
  
  //----------------------------------------------------
  
  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = new Number(0)
    FunCall(sub, List(zero, exp))
  }

  def invert(exp: Expression): Expression = {
    val sub = Identifier("div")
    val one = new Number(1)
    FunCall(sub, List(one, exp))
  }
}