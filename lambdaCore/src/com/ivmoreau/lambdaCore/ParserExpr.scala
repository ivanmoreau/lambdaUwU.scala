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
  * Type :=
  * | TypeVariable -- Type variable (uppercase)
  * | Type "=>" Type -- Arrow type.
  * | "(" Type ")"
  * | Type Type -- Type application.
  * | Lambda TypeVariable ":" Kind "->" Type -- Type Abstraction.
  * | "forall" TypeVariable ":" Kind "."  Type -- Universal type quantification.
  * 
  * Expression :=
  * | "#" TypeVariable ":" Kind "->" Expression -- Type abstraction.
  * | TermVariable -- Variable (initial lowercase).
  * | Lambda TermVariable ":" Type "->" Expression -- Abstraction.
  * | Expression Expression -- Application.
  * | "(" Expression ")"
  *
  * Declaration :=
  * | "let" TermVariable ":=" Expression "." Declaration*
  * | "type" TypeVariable ":=" Type "." Declaration*
  * | EndOfInput
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

  lazy val parametersT: P[Seq[(String, EKind)]] =
    (variableUpper <* ofType, kind)
      .mapN { (a, b) =>
        println(s"param: $a, $b")
        (a, b) }
      .repSep(comma.surroundedBy(whitespace))
      .map(_.toList.toSeq) <* whitespace
      .withContext("parametersT")

  lazy val kind: P[EKind] = P.recursive[EKind] { rec =>
    val universe: P[EKind] = uni.map(_ => EKind.Star)
    val arrow: P[EKind] = (universe, arrowK, rec).mapN { (a, _, b) =>
      EKind.~*>(a, b)
    }

    universe | arrow
  }

  lazy val `type`: P[EType] = P.recursive[EType] { rec =>
    val variable: P[EType] = variableUpper.map(EType.TFree(_))

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

    val arrowP: P[EType] = (application, arrowT, rec).mapN { (a, _, b) =>
      EType.~+>(a, b)
    }

    arrowP.backtrack | abstraction | forallP | application
  }

  // TODO: increment the counter in recursive calls
  extension (rec: Expr)
    def mapRecE(f: PartialFunction[Expr, Expr]): Expr = rec match
      case f(replaced) => replaced
      case Expr.Lam(k, e) => Expr.Lam(k, e.mapRecE(f))
      case Expr.Abs(t, e) => Expr.Abs(t, e.mapRecE(f))
      case Expr.App(a, b) => Expr.App(a.mapRecE(f), b.mapRecE(f))
      case otherwise => otherwise

    def mapRecT(f: PartialFunction[EType, EType]): Expr = rec match
      case Expr.Lam(k, e) => Expr.Lam(k, e.mapRecT(f))
      case Expr.Abs(t, e) => Expr.Abs(t.mapRecT(f), e.mapRecT(f))
      case Expr.App(a, b) => Expr.App(a.mapRecT(f), b.mapRecT(f))
      case otherwise => otherwise

  extension (rec: EType)
    def mapRecT(f: PartialFunction[EType, EType]): EType = rec match
      case f(replaced) => replaced
      case EType.TAbs(k, t) => EType.TAbs(k, t.mapRecT(f))
      case EType.TApp(l, r) => EType.TApp(l.mapRecT(f), r.mapRecT(f))
      case EType.FAll(k, t) => EType.FAll(k, t.mapRecT(f))
      case EType.~+>(p, e) => EType.~+>(p.mapRecT(f), e.mapRecT(f))
      case otherwise => otherwise

  def replaceFree(map: Map[String, Expr]) = {
    case Expr.Free(v) if map.contains(v) => map(v)
  }: PartialFunction[Expr, Expr]

  def replaceFreeT(map: Map[String, EType]) = {
    case EType.TFree(v) if map.contains(v) => map(v)
  }: PartialFunction[EType, EType]

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
            .traverse(params.reverse.map(_._1))(bruijn[Expr.Var](Expr.Var(_)))
            .runS(Map.empty)
            .value
          val preResult = params.foldRight(body) {
            case ((_, Some(param)), body) => Abs(param, body)
            case ((_, None), body) => Abs(EType.Any, body)
          }
          preResult.mapRecE(replaceFree(values))
      }.withContext("abstraction")

    lazy val typeAbstraction: P[Expr] =
      (lambdaT *> parametersT <* arrow, rec.backtrack).mapN {
        (params, body) =>
          val values: Map[String, EType.TVar] = Traverse[Seq]
            .traverse(params.reverse.map(_._1))(bruijn[EType.TVar](EType.TVar(_)))
            .runS(Map.empty)
            .value
          println(values)
          val preResult = params.foldRight(body) { case ((_, param), body) => Lam(param, body) }
          preResult.mapRecT(replaceFreeT(values))
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
