package com.ivmoreau.lambdaCore

import cats.data.{NonEmptyList, State}
import cats.implicits.*
import cats.instances.string
import cats.{Eval, Traverse}
import cats.parse.{Parser as P, Parser0 as P0, *}
import cats.syntax.all.*
import com.ivmoreau.lambdaCore.Expr.*
import mouse.all.*

import scala.util.Try

/** Gramar
  *
  * Expression :=
  * | TermVariable -- Variable (initial lowercase).
  * | Lambda TermVariable "->" Expression -- Abstraction.
  * | Expression Expression -- Application.
  * | "(" Expression ")"
  *
  * Declaration :=
  * | "let" TermVariable ":=" Expression "." Declaration*
  * | EndOfInput
  *
  */
class ParserExpr(
    val input: String,
    val extensions: Extensions,
    val reservedWords: List[String] = List("\\")
):

  def parseInput: Try[Expr] =
    val either = expression.parseAll(input) match
      case Right(value) => Right(value)
      case Left(error)  => Left(new Exception(error.toString))
    either.toTry

  def parseContext: Try[Seq[Decl]] =
    val either = context.parseAll(input) match
      case Right(value) => Right(value)
      case Left(error)  => Left(new Exception(error.toString))
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
  lazy val arrow = P.string("->") <* whitespace

  lazy val naturals: P[Expr] = extensions.useNativeNats match
    case false => P.failWith("Naturals are not enabled")
    case true  => digits.map(a => Expr.DVal(KType.KVal(LType.LInt(a))))

  lazy val variableExpr: P[Expr] = variableLower.map(Free(_))

  lazy val parameters: P[Seq[String]] =
      variableLower
      .repSep(comma.surroundedBy(whitespace))
      .map(_.toList) <* whitespace

  // TODO: increment the counter in recursive calls
  extension (rec: Expr)
    def mapRecE(f: PartialFunction[Expr, Expr]): Expr = rec match
      case f(replaced) => replaced
      case Expr.Abs(e) => Expr.Abs(e.mapRecE(f))
      case Expr.App(a, b) => Expr.App(a.mapRecE(f), b.mapRecE(f))
      case otherwise => otherwise

  def replaceFree(map: Map[String, Expr]) = {
    case Expr.Free(v) if map.contains(v) => map(v)
  }: PartialFunction[Expr, Expr]

  // Bruijn
  def bruijn[T](f: Int => T)(varName: String): State[Map[String, T], T] =
    State[Map[String, T], T] { (s: Map[String, T]) =>
      s.get(varName) match
        // If already indexed, leave the map unchanged and return the index
        case Some(idx) => (s, idx)
        // If not already indexed, use the next index (s.size) for the Var,
        // add it to the Map in state and return both
        case None =>
          val newElem: T = f(s.size)
          val updated: Map[String, T] = s + (varName -> newElem)
          (updated, newElem)
    }

  lazy val expression: P[Expr] = P.recursive[Expr] { rec =>
    lazy val abstraction: P[Expr] =
      (lambda.backtrack *> parameters <* arrow, rec.backtrack).mapN {
        (params, body) =>
          val values: Map[String, Expr.Var] = Traverse[Seq]
            .traverse(params.reverse)(bruijn[Expr.Var](Expr.Var(_)))
            .runS(Map.empty)
            .value
          val preResult = params.foldRight(body) {
            case (_, body) => Abs(body)
          }
          preResult.mapRecE(replaceFree(values))
      }.withContext("abstraction")

    lazy val atom: P[Expr] = variableExpr | naturals

    val application: P[Expr] = (abstraction | atom.backtrack | rec.between(
      open_parentheses,
      close_parentheses
    )).rep.map { case NonEmptyList(head, tail) =>
      tail.foldLeft(head) { (acc, expr) => App(acc, expr) }
    }.withContext("application")

    abstraction | application
  }
