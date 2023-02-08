package com.ivmoreau.lambda

import Expr.*
import cats.data.NonEmptyList
import org.parboiled2.*

class ParserExpr(val input: ParserInput, val extensions: Extensions, val reservedWords: List[String] = List("\\")) extends Parser:
  def unfree(expr: Expr, s: Map[String, Int]): Expr =
    expr match
      case x@Free(v) => s.get(v) match
        case None => x
        case Some(value) => Var(value)
      case Abs(t, e) => Abs(t, unfree(e, s.map((s, i) => (s, i + 1))))
      case App(l, r) => App(unfree(l, s), unfree(r, s))
      case var_ => var_

  def app(expr: Seq[Expr]): Expr = expr.length match
    case 1 => expr.head
    case 2 => App(expr.head, expr(1))
    case n => expr.tail.foldLeft(expr.head)((head, new_) => App(head, new_))

  def InputLine: Rule1[Expr] = rule { Expression ~ quiet(EOI) }

  def WS: Rule0 = rule { quiet(zeroOrMore(anyOf(" \t \n"))) }
  def Variable: Rule1[String] = rule {
    capture(CharPredicate.LowerAlpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~ WS
  }
  def VariableExpr: Rule1[Expr] = rule {
    Variable ~> { (a: String) =>
      Expr.Free(a)
    }
  }

  def Digits: Rule1[Int] = rule { capture(oneOrMore {
    CharPredicate.Digit
  }) ~ WS ~> ((str: String) => str.toInt)}
  def Natural: Rule1[Expr] = rule {
    test(extensions.useNativeNats) ~ Digits ~> ((d: Int) => DVal(d))
  }

  def AtomExpr: Rule1[Expr] = rule {
    VariableExpr | Natural
  }

  def Comma: Rule0 = rule { "," ~ WS }
  def ParensExpr: Rule1[Expr] = rule { "(" ~ WS ~ Expression ~ ")" ~ WS }
  def Lambda: Rule0 = rule { "\\" | "λ" }

  def Program: Rule1[Seq[Decl]] = rule {
    WS ~ zeroOrMore(Declaration) ~ EOI
  }

  def WordsToRules(words: List[String]): List[Rule1[String]] = words.map{ w =>
    rule { capture(str(w)) ~> { (a: String) => println(s"cap $a"); a } }
  }

  def Alternatives[A](rules: List[Rule1[A]]): Rule1[A] = rules.reduce((a, b) => rule(a | b))

  def MatchAnyString(words: List[String]): Rule1[String] = Alternatives(WordsToRules(words))

  def VariableDecl: Rule1[String] = rule {
    !(MatchAnyString(reservedWords) ~ !CharPredicate.AlphaNum ~ WS ~ ":=") ~ Variable
  }

  def Declaration: Rule1[Decl] = rule {
    VariableDecl ~ ":=" ~ WS ~ Expression ~ "." ~ WS ~> {(a: String, b: Expr) => Decl.Bind(a, b)}
  }

  def Expression: Rule1[Expr] = rule {
    Abstraction | Application ~ WS
  }
  def Abstraction: Rule1[Expr] = rule {
    Lambda ~ WS ~ Variable.+(Comma) ~ "->" ~ WS ~ Expression ~> { (xs: Seq[String], b: Expr) =>
      val map = xs.foldRight((Map.empty[String, Int], 0))((s, t) => t match
        case (m, c) => (m + ((s, c)), c + 1)
      )._1
      xs.foldLeft(unfree(b, map))((abs, str) => Abs(EType.Any, abs))
    }
  }
  def Application: Rule1[Expr] = rule {
    oneOrMore(Abstraction | AtomExpr | ParensExpr) ~> ((a: Seq[Expr]) => app(a))
  }

