package com.ivmoreau.lambdaCore

import scala.util.{Failure, Success, Try}
import org.parboiled2.*

import scala.annotation.tailrec
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

import cats.Applicative.*
import cats.implicits.*

val subZero: (Int, Int) => Int =
  case (n, m) => if n - m < 0 then 0 else n - m

enum LType:
  case LInt(val n: Int)
  case LBoolean(val n: Boolean)
  override def toString(): String = this match
    case LInt(n) => n.toString()
    case LBoolean(n) => n.toString()

enum KType:
  case KVal(val n: LType)
  import LType.*
  def getInt: Option[Int] = this match
    case KVal(LInt(n)) => Some(n)
    case otherwise => None
  def getBoolean: Option[Boolean] = this match
    case KVal(LBoolean(n)) => Some(n)
    case otherwise => None
  override def toString(): String = this match
    case KVal(n) => n.toString()
  
    
given fromIntToKType: Conversion[Int, KType] = a => KType.KVal(LType.LInt(a))
given fromBoolToKType: Conversion[Boolean, KType] = a => KType.KVal(LType.LBoolean(a))
  
val natFnExtensions: Map[String, Expr] = Map.from(List(
  Expr.DFun("succ", (a: KType) => a.getInt.map { (a: Int) => Expr.DVal(a + 1) }),
  Expr.DFun("add", (a: KType) => a.getInt.map { (a: Int) =>
    Expr.DFun("add$Partial", (b: KType) => b.getInt.map { (b: Int) => Expr.DVal(a + b) } ) 
  }),
  Expr.DFun("sub", (a: KType) => a.getInt.map { (a: Int) => 
    Expr.DFun("sub$Partial", (b: KType) => b.getInt.map { (b: Int) => Expr.DVal(subZero(a, b)) } ) 
  })
).map {
  case e@Expr.DFun(n, f) => (n, e)
  case _ => ("impossible", Expr.DVal(0))
})

val boolFnExtensions: Map[String, Expr] = Map.from(List(
  Expr.DFun("if", (a: KType) => a.getBoolean.map { (a: Boolean) => Expr.Abs(EType.Any, Expr.Abs(EType.Any, {
    if a then Expr.Var(1) else Expr.Var(0)
  }))})
).map {
  case e@Expr.DFun(n, f) => (n, e)
  case _ => ("impossible", Expr.DVal(0))
}) ++ Map.from(List(("true", Expr.DVal(true)), ("false", Expr.DVal(false))))

val boolNatFnExtension: Map[String, Expr] = Map.from(List(
  Expr.DFun("eqN", (a: KType) => a.getInt.map { (a: Int) => 
    Expr.DFun("eqN$Partial", (b: KType) => b.getInt.map { (b: Int) => Expr.DVal(a == b) }) 
  })
).map {
  case e@Expr.DFun(n, f) => (n, e)
  case _ => ("impossible", Expr.DVal(0))
})

case class Extensions(
  useNativeNats: Boolean = false,
  useNativeBools: Boolean = false,
  systemFOmega: Boolean = false
)

def reservedWords(extensions: Extensions): List[String] =
  def iff(b: Boolean, e: Map[String, Expr]): List[String] =
    if b then e.keys.toList else List("")
  val natE = iff(extensions.useNativeNats, natFnExtensions)
  val boolE = iff(extensions.useNativeBools, boolFnExtensions)
  val natBoolE = iff(extensions.useNativeBools && extensions.useNativeNats, boolNatFnExtension)
  List("\\") ++ natE ++ boolE ++ natBoolE

def contextAdditions(extensions: Extensions): Map[String, Expr] =
  def iff(b: Boolean, e: Map[String, Expr]): Map[String, Expr] =
    if b then e else Map()
  val natE = iff(extensions.useNativeNats, natFnExtensions)
  val boolE = iff(extensions.useNativeBools, boolFnExtensions)
  val natBoolE = iff(extensions.useNativeBools && extensions.useNativeNats, boolNatFnExtension)
  natE ++ boolE ++ natBoolE

extension (str: String)
  def evaluate(ctx: String)(extensions: Extensions): String =
    //println(reservedWords(extensions))
    val ctx_instance = new ParserExpr(ctx, extensions, reservedWords(extensions))
    val input_instance = new ParserExpr(str, extensions, reservedWords(extensions))
    val input_n: Try[Expr] = input_instance.parseInput
    if input_n.isFailure then return s"Parse error: ${input_n.failed.get.getMessage}"
    val ctx_n: Try[Seq[Decl]] = ctx_instance.parseContext
    if ctx_n.isFailure then return s"Parse error: ${ctx_n.failed.get.getMessage}"
    //println(s"$ctx")
    def result[A, B](v: Try[A])(f: A => B): Either[String, B] = v match
      case Success(value) => Right(f(value))
      case Failure(exception) => Left( s"Parse error: ${exception.getMessage}" )
    val decl2Tuple: Decl => (String, Expr) =
      case Decl.Bind(s, e) => (s, e)
    val a = result(ctx_n)(_.map(decl2Tuple).toMap)
    val b = result(input_n)(identity)
    //println(b)
    val almost =
      for
        iExpr <- b
        cExprs <- a
      yield Evaluator(iExpr, cExprs ++ contextAdditions(extensions)).eval()
    almost match
      case Left(value) => value
      case Right(value) =>
        /*for
         a <- value.map(PrinterExpr(_, noIdent = true).print())
        yield println(a)*/
        PrinterExpr(value.head).print()


enum EKind:
  case Star
  case ~*>(val p: EKind, val e: EKind)

enum EType:
  case TFree(val s: String)
  case TVar(val x: Int)
  case TAbs(val k: EKind, val t: EType)
  case TApp(val l: EType, val r: EType)
  case FAll(val k: EKind, val t: EType)
  case ~+>(val p: EType, val e: EType)
  case Any

enum Expr:
  case Free(val s: String)
  case Var(val x: Int)
  case Abs(val t: EType, val e: Expr)
  case App(val l: Expr, val r: Expr)
  case Lam(val k: EKind, val e: Expr)
  case DVal(val d: KType)
  case DFun(val name: String, val f: KType => Option[Expr])

enum Decl:
  case Bind(val name: String, val expr: Expr)
  case BindT(val name: String, val ttype: EType)

case class PrinterExpr(expr: Expr, ident: Int = 0, noIdent: Boolean = false):
  def print(): String =
    val identation = if (noIdent) {
      ("", "")
    } else {
      (" ".repeat(ident), "\n")
    }
    val newExpr: String = expr match
      case Expr.Free(f) => f
      case Expr.Var(x) => x.toString
      case Expr.Abs(t, e) => s"λ ->${identation._2}${this.copy(expr = e, ident = ident + 2).print()}"
      case Expr.App(l, r) => s"(${this.copy(expr = l).print()} ${this.copy(expr = r).print()})"
      case Expr.DVal(v) => v.toString()
      case Expr.DFun(n, _) => n
    identation._1 + newExpr

case class Evaluator(expr: Expr, ctx: Map[String, Expr]):
  import Expr._

  val shift: (Int, Int, Expr) => Expr =
    case (_, _, Free(n)) => Free(n)
    case (d, c, Var(n)) => if c <= n then Var(n + d) else Var(n)
    case (d, c, App(l, r)) => App(shift(d, c, l), shift(d, c, r))
    case (d, c, Abs(t, e)) => Abs(t, shift(d, c + 1, e))
    case (_, _, t) => t

  val unshift: (Int, Int, Expr) => Expr =
    case (_, _, Free(n)) => Free(n)
    case (d, c, Var(n)) => if c <= n then Var(n - d) else Var(n)
    case (d, c, App(l, r)) => App(unshift(d, c, l), unshift(d, c, r))
    case (d, c, Abs(t, e)) => Abs(t, unshift(d, c + 1, e))
    case (_, _, t) => t

  val substitution: (Expr, Expr, Int) => Expr =
    case (Free(v), _, _) => Free(v)
    case (Var(n), e, m) => if n == m then e else Var(n)
    case (App(e1, e2), e, m) => App(substitution(e1, e, m), substitution(e2, e, m))
    case (Abs(t, e1), e, m) => Abs(t, substitution(e1, shift(1, 0, e), m + 1))
    case (t, _, _) => t

  val betaB: Expr => Expr =
    case App(Abs(t, t1), t2) => unshift(1, 0, substitution(t1, shift(1, 0, t2), 0))
    case App(DFun(n, f), DVal(v)) => f(v) match
      case None => App(DFun(n, f), DVal(v))
      case Some(value) => value
    case App(DFun(n, f), e) => App(DFun(n, f), betaB(e))
    case App(Var(n), t2) => App(Var(n), betaB(t2))
    case App(Free(v), t2) => ctx.get(v) match
      case Some(value) => App(value, t2)
      case None => App(Free(v), t2)
    case App(t1, t2) => App(betaB(t1), t2)
    case Abs(t, e) => Abs(t, betaB(e))
    case Var(n) => Var(n)
    case Free(n) => ctx.get(n) match
      case Some(value) => value
      case None => Free(n)
    case n => n

  @tailrec
  private def betaApply(x: Expr, xs: List[Expr]): List[Expr] =
    val u = betaB(x)
    x == u match
      case true => if xs.isEmpty then x :: xs else xs
      case false => betaApply(u, u :: xs)

  val eval: () => List[Expr] =
    case _ => betaApply(this.expr, List())