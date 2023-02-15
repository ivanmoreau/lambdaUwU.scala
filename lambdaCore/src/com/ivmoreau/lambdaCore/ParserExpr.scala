package com.ivmoreau.lambdaCore

import Expr.*
import cats.data.NonEmptyList
import org.parboiled2.*
import mouse.any

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

  def appT(ty: Seq[EType]): EType = ty.reduceLeft(EType.TApp.apply)

  def InputLine: Rule1[Expr] = rule { Expression ~ quiet(EOI) }

  def WS: Rule0 = rule { quiet(zeroOrMore(anyOf(" \t \n"))) }
  def Variable: Rule1[String] = rule {
    capture(CharPredicate.LowerAlpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~ WS
  }

  def VariableType: Rule1[String] = rule {
    capture(CharPredicate.UpperAlpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~ WS
  }

  def VariableExpr: Rule1[Expr] = rule {
    Variable ~> { (a: String) =>
      Expr.Free(a)
    }
  }

  def VariableExprType: Rule1[EType] = rule {
    VariableType ~> { (a: String) =>
      EType.TFree(a)  
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
  def AtomType: Rule1[EType] = rule {
    VariableExprType
  }

  def Comma: Rule0 = rule { "," ~ WS }
  def Colon: Rule0 = rule { "*" ~ WS }
  def Parens[A](e: () => Rule1[A]): Rule1[A] = rule { "(" ~ WS ~ e() ~ ")" ~ WS }
  def Lambda: Rule0 = rule { ("\\" | "λ") ~ WS }
  def LambdaET: Rule0 = rule { raw"#" ~ WS } // TODO: Uppercase lambda
  def Let: Rule0 = rule { "let" ~ WS }
  def Type: Rule0 = rule { "type" ~ WS }

  def Program: Rule1[Seq[Decl]] = rule {
    WS ~ zeroOrMore(Declaration) ~ EOI
  }

  def WordsToRules(words: List[String]): List[Rule1[String]] = words.map{ w =>
    rule { capture(str(w)) ~> { (a: String) => /*println(s"cap $a");*/ a } }
  }

  def Alternatives[A](rules: List[Rule1[A]]): Rule1[A] = rules.reduce((a, b) => rule(a | b))

  def MatchAnyString(words: List[String]): Rule1[String] = Alternatives(WordsToRules(words))

  def VariableDecl: Rule1[String] = rule {
    !(MatchAnyString(reservedWords) ~ !CharPredicate.AlphaNum ~ WS ~ ":=") ~ Variable
  }

  def VariableTypeDecl: Rule1[String] = rule {
    !(MatchAnyString(reservedWords) ~ !CharPredicate.AlphaNum ~ WS ~ ":=") ~ VariableType
  }

  def DeclarationValue: Rule1[Decl] = rule {
    Let ~ VariableDecl ~ ":=" ~ WS ~ Expression ~ "." ~ WS ~> {(a: String, b: Expr) => Decl.Bind(a, b)}
  }

  def DeclarationType: Rule1[Decl] = rule {
    Type ~ VariableTypeDecl ~ ":=" ~ WS ~ TypeExpression ~ "." ~ WS ~> {(a: String, b: EType) => Decl.BindT(a, b)}
  }

  def TypeExpression: Rule1[EType] = rule {
    TypeAbstraction | TypeForall | TypeApplication
  }

  def Declaration: Rule1[Decl] = rule { DeclarationValue | DeclarationType }

  def Expression: Rule1[Expr] = rule {
    AbstractionT | Abstraction | Application ~ WS
  }

  def rest: Rule1[String] = rule { capture(zeroOrMore(CharPredicate.All)) }
  def CaptureRest: Rule1[String] = rule { rest ~ EOI ~> { (a: String) => println(a); a}}
  def Debug(text: => String) = rule { test({println(s"DEBUG: $text"); true}) }

  def Star: Rule1[EKind] = rule { capture(str("*")) ~ WS ~> { (_: String) => EKind.Star }}

  def Kind: Rule1[EKind] = rule {
    Star.+("~>" ~ WS) ~> { (a: Seq[EKind]) => a.reduceRight { EKind.~*>.apply } }
  }

  def TypeAbstraction: Rule1[EType] = rule { capture("DFDFFDFD") ~> {(_: String) => EType.Any} }
  def TypeForall: Rule1[EType] = rule { capture("DFDFFDFD") ~> {(_: String) => EType.Any} }
  def TypeExprAbstractionT: Rule1[EType] = rule {
    TypeExpression.+("->" ~ WS) ~> { (ty: Seq[EType]) => ty.reduceRight(EType.~+>.apply) }
  }

  def TypeApplication: Rule1[EType] = rule {
    oneOrMore(TypeExprAbstractionT | TypeAbstraction | TypeForall | Parens(() => TypeExpression)) ~> { (xs: Seq[EType]) =>
      appT(xs)
    }
  }

  def Parameter[A](v: () => Rule1[String], kt: () => Rule1[A]): Rule1[(String, A)] = rule {
    v() ~ ":" ~ WS ~ kt() ~> { (name: String, kind: A) => println(s"HERE0 $name $kind"); (name, kind) }
  }

  def Abstraction: Rule1[Expr] = rule {
    Lambda ~ Parameter(() => Variable, () => TypeExpression).+(Comma) ~ "->" ~ WS ~ Expression ~> { (xs: Seq[(String, EType)], b: Expr) =>
      val map = xs.foldRight((Map.empty[String, Int], 0))((s, t) => t match
        case (m, c) => (m + ((s._1, c)), c + 1)
      )._1
      xs.foldLeft(unfree(b, map))((abs, str) => Abs(EType.Any, abs))
    }
  }
  def AbstractionT: Rule1[Expr] = rule {
    LambdaET ~ Parameter(() => VariableType, () => Kind).+(Comma) ~ "->" ~ WS ~ Expression ~> { (xs: Seq[(String, EKind)], b: Expr) =>
      println(s"HERE $xs $b")
      Free("???")
    }
  }
  def Application: Rule1[Expr] = rule {
    oneOrMore(Abstraction | AtomExpr | Parens(() => Expression)) ~> ((a: Seq[Expr]) => app(a))
  }

