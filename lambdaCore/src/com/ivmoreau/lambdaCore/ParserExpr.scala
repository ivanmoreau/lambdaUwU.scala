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
    val either = context.parseAll(input) match
      case Right(value) => Right(value)
      case Left(error) => Left(new Exception(error.toString))
    either.toTry

  lazy val context: P0[Seq[Decl]] = (declaration <* P.end)

  lazy val declaration: P0[Seq[Decl]] = expressionDecl.repSep0(P.char(';').surroundedBy(whitespace)).map(_.toList.toSeq)

  lazy val expressionDecl: P[Decl] = (variableLower <* definedAs, expression).mapN(Decl.Bind(_,_))
  
  lazy val variableLower: P[String] = (P.charWhere(_.isLower) ~ P.charWhere(_.isLetterOrDigit).rep0).string <* whitespace
  lazy val variableUpper: P[String] = (P.charWhere(_.isUpper) ~ P.charWhere(_.isLetterOrDigit).rep0).string <* whitespace
  
  lazy val lambda: P[Unit] = (P.char('\\') | P.char('λ')) <* whitespace
  lazy val dot: P[Unit] = P.char('.') <* whitespace
  lazy val comma: P[Unit] = P.char(',') <* whitespace
  lazy val ofType: P[Unit] = P.char(':') <* whitespace
  lazy val whitespace: P0[Unit] = P.charWhere(_.isWhitespace).rep0.void
  lazy val definedAs: P[Unit] = P.string(":=") <* whitespace
  lazy val open_parentheses: P[Unit] = P.char('(') <* whitespace
  lazy val close_parentheses: P[Unit] = P.char(')') <* whitespace
  lazy val digits: P[Int] = Rfc5234.digit.rep.string.mapFilter(s => Try(s.toInt).toOption) <* whitespace

  lazy val naturals: P[Expr] = extensions.useNativeNats match
    case false => P.failWith("Naturals are not enabled")
    case true => digits.map(a => Expr.DVal(KType.KVal(LType.LInt(a))))

  lazy val variableExpr: P[Expr] = variableLower.map(Free(_))

  lazy val parameters: P[Seq[String]] = variableLower.repSep(comma.surroundedBy(whitespace)).map(_.toList.toSeq) <* whitespace

  /* EXAMPLE
  --------- GRAMAR λ ---------

expr = whiteSpaces >> (abstraction <|> application)
abstraction = try (lambda (sepBy1 isVar_ comma) period (try expr))
application = (chainl1 (abstraction <|> variable <|> (open_parentheses expr close_parentheses)) (do return (\x₀ -> \x₁ -> App x₀ x₁)))

-- λn,sd,ds,s.ss (sd df) --> Right (λn.(λsd.(λds.(λs.(ss (sd df))))))
*/

  /* NEW CODE based on the Haskell code */
  lazy val expression: P[Expr] = P.recursive[Expr] { rec =>
    val abstraction: P[Expr] = (lambda.backtrack *> parameters <* dot, rec.backtrack).mapN { (params, body) =>
      params.foldRight(body) { (param, body) => Abs(EType.Any, body) }
    }

    val atom: P[Expr] = variableExpr | naturals

    val application: P[Expr] = (abstraction | atom.backtrack | rec.between(open_parentheses, close_parentheses)).rep.map {
      case NonEmptyList(head, tail) => 
        tail.foldLeft(head) { (acc, expr) => App(acc, expr) }
    }
    
    abstraction | application
  }

