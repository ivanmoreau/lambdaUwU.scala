package com.ivmoreau.lambdaCore

import Expr.*
import cats.implicits.*
import cats.data.NonEmptyList
import mouse.all.*
import cats.parse.{Parser => P, Parser0 => P0, *}
import cats.instances.string
import cats.syntax.all.*
import scala.util.Try

/** Gramar
  *
  * Expression :=
  * \| "#" TypeVariable ":" Kind "->" Expression -- Type abstraction.
  * \| TermVariable -- Variable (initial lowercase).
  * \| Lambda TermVariable ":" Type "->" Expression -- Abstraction.
  * \| Expression Expression -- Application.
  * \| "(" Expression ")"
  *
  * Declaration :=
  * \| "let" TermVariable ":=" Expression "." Declaration*
  * \| "type" TypeVariable ":=" Type "." Declaration*
  * \| EndOfInput
  *
  * Kind :=
  * \| "*" -- Universe.
  * \| Kind "~>" Kind
  */
class ParserExpr(
    val input: String,
    val extensions: Extensions,
    val reservedWords: List[String] = List("\\")
):

  def parseInput: Try[Expr] =
    val either = expression.parseAll(input) match
      case Right(value) => Right(value)
      case Left(error)  => Left(new Exception(show"$error"))
    either.toTry

  def parseContext: Try[Seq[Decl]] =
    val either = context.parseAll(input) match
      case Right(value) => Right(value)
      case Left(error)  => Left(new Exception(show"$error"))
    either.toTry

  lazy val context: P0[Seq[Decl]] = (declaration <* P.end)

  lazy val declaration: P0[Seq[Decl]] =
    expressionDecl.repSep0(dot).map(_.toList.toSeq)

  lazy val expressionDecl: P[Decl] =
    (variableLower.filter(!reservedWords.contains(_)) <* definedAs, expression)
      .mapN(Decl.Bind(_, _))

  lazy val variableLower: P[String] = (P.charWhere(_.isLower) ~ P
    .charWhere(_.isLetterOrDigit)
    .rep0).string <* whitespace
  lazy val variableUpper: P[String] = (P.charWhere(_.isUpper) ~ P
    .charWhere(_.isLetterOrDigit)
    .rep0).string <* whitespace

  lazy val lambda: P[Unit] = (P.char('\\') | P.char('λ')) <* whitespace
  lazy val dot: P[Unit] = P.char('.') <* whitespace
  lazy val comma: P[Unit] = P.char(',') <* whitespace
  lazy val ofType: P[Unit] = P.char(':') <* whitespace
  lazy val whitespace: P0[Unit] = P.charWhere(_.isWhitespace).rep0.void
  lazy val definedAs: P[Unit] = P.string(":=") <* whitespace
  lazy val open_parentheses: P[Unit] = P.char('(') <* whitespace
  lazy val close_parentheses: P[Unit] = P.char(')') <* whitespace
  lazy val digits: P[Int] =
    Rfc5234.digit.rep.string.mapFilter(s => Try(s.toInt).toOption) <* whitespace
  lazy val uni = P.char('*') <* whitespace
  lazy val arrowK = P.string("~>") <* whitespace
  lazy val arrowT = P.string("=>") <* whitespace
  lazy val forall = P.string("forall") <* whitespace
  lazy val lambdaT = P.string("#") <* whitespace
  lazy val arrow = P.string("->") <* whitespace

  lazy val naturals: P[Expr] = extensions.useNativeNats match
    case false => P.failWith("Naturals are not enabled")
    case true  => digits.map(a => Expr.DVal(KType.KVal(LType.LInt(a))))

  lazy val variableExpr: P[Expr] = variableLower.map(Free(_))

  lazy val parameters: P[Seq[(String, Option[EType])]] = 
    if !extensions.systemFOmega then
      variableLower
      .repSep(comma.surroundedBy(whitespace))
      .map(_.toList.toSeq.map((_, None))) <* whitespace
    else
      (variableLower <* ofType, `type`)
        .mapN { (a, b) => 
          println(s"param: $a, $b")
          (a, Some(b)) }
        .repSep(comma.surroundedBy(whitespace))
        .map(_.toList.toSeq) <* whitespace

  lazy val parametersT: P[Seq[(String, EType)]] = 
    (variableUpper <* ofType, `type`)
      .mapN { (a, b) => 
        println(s"param: $a, $b")
        (a, b) }
      .repSep(comma.surroundedBy(whitespace))
      .map(_.toList.toSeq) <* whitespace

  lazy val kind: P[EKind] = P.recursive[EKind] { rec =>
    val universe: P[EKind] = uni.map(_ => EKind.Star)
    val arrow: P[EKind] = (universe, arrowK, rec).mapN { (a, _, b) =>
      EKind.~*>(a, b)
    }

    universe | arrow
  }

  lazy val `type`: P[EType] = P.recursive[EType] { rec =>
    val variable: P[EType] = variableUpper.map(EType.TFree(_))
    val arrow: P[EType] = (rec, arrowT, rec).mapN { (a, _, b) =>
      EType.~+>(a, b)
    }
    val abstraction: P[EType] =
      (lambda *> variableUpper <* ofType, kind, arrow, rec).mapN {
        (_, a, _, b) => EType.TAbs(a, b)
      }
    val forallP: P[EType] =
      (forall *> variableUpper <* ofType, kind, rec).mapN { (v, k, a) => ??? }
    val application: P[EType] =
      (abstraction | forallP | variable.backtrack | rec.between(
        open_parentheses,
        close_parentheses
      )).rep.map { case NonEmptyList(head, tail) =>
        tail.foldLeft(head) { (acc, expr) => EType.TApp(acc, expr) }
      }

    arrow.backtrack | abstraction | forallP | application
  }

  lazy val expression: P[Expr] = P.recursive[Expr] { rec =>
    lazy val abstraction: P[Expr] =
      (lambda.backtrack *> parameters <* arrow, rec.backtrack).mapN {
        (params, body) =>
          params.foldRight(body) { (param, body) => Abs(EType.Any, body) }
      }.withContext("abstraction")

    lazy val typeAbstraction: P[Expr] =
      (lambdaT *> parametersT <* arrow, rec.backtrack).mapN {
        (params, body) =>
          params.foldRight(body) { (param, body) => Abs(EType.Any, body) }
      }.withContext("typeAbstraction")

    lazy val atom: P[Expr] = variableExpr | naturals

    val application: P[Expr] = (abstraction | typeAbstraction | atom.backtrack | rec.between(
      open_parentheses,
      close_parentheses
    )).rep.map { case NonEmptyList(head, tail) =>
      tail.foldLeft(head) { (acc, expr) => App(acc, expr) }
    }.withContext("application")

    abstraction | typeAbstraction | application
  }
