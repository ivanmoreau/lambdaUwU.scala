package com.ivmoreau.lambdaCore

import Expr.*
import cats.data.NonEmptyList
import mouse.all.*
import cats.parse.{Parser => P, Parser0 => P0, *}
import cats.instances.string
import cats.syntax.all.*
import scala.util.Try

class ParserExpr(val input: String, val extensions: Extensions, val reservedWords: List[String] = List("\\")):

  def parseInput: Try[Expr] =
    val either = expression.parseAll(input) match
      case Right(value) => Right(value)
      case Left(error) => Left(new Exception(error.toString))
    either.toTry

  def parseContext: Try[Seq[Decl]] =
    println("Parsing context")
    val either = context.parseAll(input) match
      case Right(value) => Right(value)
      case Left(error) => Left(new Exception(error.toString))
    either.toTry

  lazy val context: P0[Seq[Decl]] = (declaration.map{ a=>
    println(s"Context: $a")
    a
  } <* P.end)

  lazy val declaration: P0[Seq[Decl]] = expressionDecl.repSep0(P.char(';').surroundedBy(whitespace)).map(_.toList.toSeq)

  lazy val expressionDecl: P[Decl] = (variableLower <* definedAs, expression).mapN(Decl.Bind(_,_))
  
  lazy val variableLower: P[String] = (P.charWhere(_.isLower) ~ P.charWhere(_.isLetterOrDigit).rep0).string <* whitespace
  lazy val variableUpper: P[String] = (P.charWhere(_.isUpper) ~ P.charWhere(_.isLetterOrDigit).rep0).string <* whitespace
  
  lazy val lambda: P[Unit] = (P.char('\\') | P.char('λ')) <* whitespace
  lazy val dot: P[Unit] = P.char('.') <* whitespace
  lazy val comma: P[Unit] = P.char(',') <* whitespace
  lazy val ofType: P[Unit] = P.char(':') <* whitespace
  lazy val whitespace: P[Unit] = P.charWhere(_.isWhitespace).rep.void
  lazy val definedAs: P[Unit] = P.string(":=") <* whitespace

  lazy val variableExpr: P[Expr] = variableLower.map(Free(_))

  lazy val parameters: P[Seq[String]] = variableLower.repSep(comma.surroundedBy(whitespace)).map(_.toList.toSeq) <* whitespace

  lazy val expression: P[Expr] = P.recursive[Expr] { rec =>
    val abstraction: P[Expr] = (lambda *> parameters <* dot, rec).mapN { (params, body) =>
      params.foldRight(body) { (param, body) => Abs(EType.Any, body) }
    }
    val application: P[Expr] = rec.repSep(whitespace).map { exprs =>
      exprs.reduceLeft { (acc, expr) => App(acc, expr) }
    }
    val atom: P[Expr] = variableExpr

    abstraction | application | atom
  }

