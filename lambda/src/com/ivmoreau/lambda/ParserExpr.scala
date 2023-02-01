package com.ivmoreau.lambda

import Expr.*
import cats.data.NonEmptyList
import org.parboiled2.*

class ParserExpr(val input: ParserInput) extends Parser:
  def unfree(expr: Expr, s: Map[String, Int]): Expr =
    expr match
      case x@Free(v) => s.get(v) match
        case None => x
        case Some(value) => Var(value)
      case Abs(e) => Abs(unfree(e, s.map((s, i) => (s, i + 1))))
      case App(l, r) => App(unfree(l, s), unfree(r, s))
      case var_ => var_

  def app(expr: Seq[Expr]): Expr = expr.length match
    case 1 => expr.head
    case 2 => App(expr.head, expr(1))
    case n => expr.tail.foldLeft(expr.head)((head, new_) => App(head, new_))

  def InputLine: Rule1[Expr] = rule { Expression ~ EOI }

  def WS: Rule0 = rule { zeroOrMore(anyOf(" \t \n")) }
  def Variable: Rule1[String] = rule {
    capture(CharPredicate.LowerAlpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~ WS
  }
  def VariableExpr: Rule1[Expr] = rule {
    Variable ~> { (a: String) =>
      Expr.Free(a)
    }
  }
  def Comma: Rule0 = rule { "," ~ WS }
  def ParensExpr: Rule1[Expr] = rule { "(" ~ WS ~ Expression ~ ")" ~ WS }
  def Lambda: Rule0 = rule { "\\" | "λ" }

  def Program: Rule1[Seq[Decl]] = rule {
    WS ~ zeroOrMore(Declaration) ~ EOI
  }

  def Declaration: Rule1[Decl] = rule {
    Variable ~ ":=" ~ WS ~ Expression ~ "." ~ WS ~> {(a: String, b: Expr) => Decl.Bind(a, b)}
  }

  def Expression: Rule1[Expr] = rule {
    Abstraction | Application ~ WS
  }
  def Abstraction: Rule1[Expr] = rule {
    Lambda ~ WS ~ Variable.+(Comma) ~ "->" ~ WS ~ Expression ~> { (xs: Seq[String], b: Expr) =>
      val map = xs.foldRight((Map.empty[String, Int], 0))((s, t) => t match
        case (m, c) => (m + ((s, c)), c + 1)
      )._1
      xs.foldLeft(unfree(b, map))((abs, str) => Abs(abs))
    }
  }
  def Application: Rule1[Expr] = rule {
    oneOrMore(Abstraction | VariableExpr | ParensExpr) ~> ((a: Seq[Expr]) => app(a))
  }

