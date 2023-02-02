package com.ivmoreau.lambda

import scala.util.{Failure, Success, Try}
import org.parboiled2.*

val subZero: (Int, Int) => Int =
  case (n, m) => if n - m < 0 then 0 else n - m

val natFnExtensions: Map[String, Expr] = Map.from(List(
  Expr.FunNat("succ", (a: Int) => Expr.Nat(a + 1)),
  Expr.FunNat("add", (a: Int) => Expr.FunNat("add$Partial", (b: Int) => Expr.Nat(a + b))),
  Expr.FunNat("sub", (a: Int) => Expr.FunNat("sub$Partial", (b: Int) => Expr.Nat(subZero(a, b))))
).map {
  case e@Expr.FunNat(n, f) => (n, e)
  case _ => ("impossible", Expr.Nat(0))
})

val boolFnExtensions: Map[String, Expr] = Map.from(List(
  Expr.FunBool("if", (a: Boolean) => Expr.Abs(Expr.Abs({
    if a then Expr.Var(1) else Expr.Var(0)
  })))
).map {
  case e@Expr.FunBool(n, f) => (n, e)
  case _ => ("impossible", Expr.Nat(0))
}) ++ Map.from(List(("true", Expr.Bool(true)), ("false", Expr.Bool(false))))

val boolNatFnExtension: Map[String, Expr] = Map.from(List(
  Expr.FunNat("eqN", (a: Int) => Expr.FunNat("eqN$Partial", (b: Int) => Expr.Bool(a == b)))
).map {
  case e@Expr.FunNat(n, f) => (n, e)
  case _ => ("impossible", Expr.Nat(0))
})

case class Extensions(
  useNativeNats: Boolean = false,
  useNativeBools: Boolean = false
)

def reservedWords(extensions: Extensions): List[String] =
  def iff(b: Boolean, e: Map[String, Expr]): List[String] =
    if b then e.keys.toList else List("\\")
  val natE = iff(extensions.useNativeNats, natFnExtensions)
  val boolE = iff(extensions.useNativeBools, boolFnExtensions)
  val natBoolE = iff(extensions.useNativeBools && extensions.useNativeNats, boolNatFnExtension)
  natE ++ boolE ++ natBoolE

def contextAdditions(extensions: Extensions): Map[String, Expr] =
  def iff(b: Boolean, e: Map[String, Expr]): Map[String, Expr] =
    if b then e else Map()
  val natE = iff(extensions.useNativeNats, natFnExtensions)
  val boolE = iff(extensions.useNativeBools, boolFnExtensions)
  val natBoolE = iff(extensions.useNativeBools && extensions.useNativeNats, boolNatFnExtension)
  natE ++ boolE ++ natBoolE

extension (str: String)
  def evaluate(ctx: String)(extensions: Extensions): String =
    val ctx_instance = new ParserExpr(ctx, extensions, reservedWords(extensions))
    val input_instance = new ParserExpr(str, extensions, reservedWords(extensions))
    val ctx_n: Try[Seq[Decl]] = ctx_instance.Program.run()
    val input_n: Try[Expr] = input_instance.InputLine.run()
    println(s"$ctx")
    def result[A, B](v: Try[A])(f: A => B)(i: ParserExpr): Either[String, B] = v match
      case Success(value) => Right(f(value))
      case Failure(exception: ParseError) => Left(i.formatError(exception))
    val decl2Tuple: Decl => (String, Expr) =
      case Decl.Bind(s, e) => (s, e)
    val a = result(ctx_n)(_.map(decl2Tuple).toMap)(ctx_instance)
    val b = result(input_n)(identity)(input_instance)
    println(b)
    val almost =
      for
        iExpr <- b
        cExprs <- a
      yield Evaluator(iExpr, cExprs ++ contextAdditions(extensions)).eval()
    almost match
      case Left(value) => value
      case Right(value) =>
        for
         a <- value.map(PrinterExpr(_, noIdent = true).print())
        yield println(a)
        PrinterExpr(value.last).print()

enum Expr:
  case Free(val s: String)
  case Var(val x: Int)
  case Abs(val e: Expr)
  case App(val l: Expr, val r: Expr)
  case Nat(val n: Int)
  case Bool(val n: Boolean)
  case FunNat(val name: String, val f: Int => Expr)
  case FunBool(val name: String, val f: Boolean => Expr)

enum Decl:
  case Bind(val name: String, val expr: Expr)

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
      case Expr.Abs(e) => s"λ ->${identation._2}${this.copy(expr = e, ident = ident + 2).print()}"
      case Expr.App(l, r) => s"(${this.copy(expr = l).print()} ${this.copy(expr = r).print()})"
      case Expr.Nat(n) => n.toString
      case Expr.Bool(n) => n.toString
      case Expr.FunNat(n, _) => n
      case Expr.FunBool(n, _) => n
    identation._1 + newExpr

case class Evaluator(expr: Expr, ctx: Map[String, Expr]):
  import Expr._

  val shift: (Int, Int, Expr) => Expr =
    case (_, _, Free(n)) => Free(n)
    case (d, c, Var(n)) => if c <= n then Var(n + d) else Var(n)
    case (d, c, App(l, r)) => App(shift(d, c, l), shift(d, c, r))
    case (d, c, Abs(t)) => Abs(shift(d, c + 1, t))
    case (_, _, t) => t

  val unshift: (Int, Int, Expr) => Expr =
    case (_, _, Free(n)) => Free(n)
    case (d, c, Var(n)) => if c <= n then Var(n - d) else Var(n)
    case (d, c, App(l, r)) => App(unshift(d, c, l), unshift(d, c, r))
    case (d, c, Abs(t)) => Abs(unshift(d, c + 1, t))
    case (_, _, t) => t

  val substitution: (Expr, Expr, Int) => Expr =
    case (Free(v), _, _) => Free(v)
    case (Var(n), e, m) => if n == m then e else Var(n)
    case (App(e1, e2), e, m) => App(substitution(e1, e, m), substitution(e2, e, m))
    case (Abs(e1), e, m) => Abs(substitution(e1, shift(1, 0, e), m + 1))
    case (t, _, _) => t

  val betaB: Expr => Expr =
    case App(Abs(t1), t2) => unshift(1, 0, substitution(t1, shift(1, 0, t2), 0))
    case App(FunNat(n, f), Nat(v)) => f(v)
    case App(FunNat(n, f), e) => App(FunNat(n, f), betaB(e))
    case App(FunBool(n, f), Bool(v)) => f(v)
    case App(FunBool(n, f), e) => App(FunBool(n, f), betaB(e))
    case App(Var(n), t2) => App(Var(n), betaB(t2))
    case App(Free(v), t2) => ctx.get(v) match
      case Some(value) => App(value, t2)
      case None => App(Free(v), t2)
    case App(t1, t2) => App(betaB(t1), t2)
    case Abs(t) => Abs(betaB(t))
    case Var(n) => Var(n)
    case Free(n) => ctx.get(n) match
      case Some(value) => value
      case None => Free(n)
    case n => n

  val betaApply: Expr => List[Expr] =
    case x =>
      val u = betaB(x)
      if x == u then
        List(x)
      else x :: betaApply(u)

  val eval: () => List[Expr] =
    case _ => betaApply(this.expr)