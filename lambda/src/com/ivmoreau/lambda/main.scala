package com.ivmoreau.lambda

import org.scalajs.dom
import com.raquo.laminar.api.L.*
import org.scalajs.dom.{console, document}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, JSGlobalScope, JSImport}
import com.ivmoreau.lambda.{Expr, ParserExpr, PrinterExpr}
import com.ivmoreau.lambda.evaluate

val promptVar = Var(initial = "\\x -> x")
val outVar = Var(initial = "n/a")
val editorVar = Var(initial = "id = \\x -> x.")

// Editor, Prompt, Context

val computeButton = button("Compute.")
val parserExprObserver = Observer[String](onNext = {str =>
  outVar.set(str)
})

def libInfo(component: String, info: List[(String, String)]) =
  val x = info.map((n, in) => li(i(n), ": ", in)).foldLeft(ul())((a, b) => a.amend(b))
  div(
    h4(component, "~~"),
    x
  )

def option(opt: String) =
  span(
    input(
      typ := "checkbox"
    ),
    opt,
  )

val rootElement = div(
  h1(
    "λ -> " + "MangoLambdaUwU"
  ),
  h2(
    "The Lambda Calculus Interpreter"
  ),
  div(
    idAttr := "prompt-zone",
    span(">"),
    input(
      idAttr := "prompt",
      onInput.mapToValue --> promptVar,
      placeholder := "\\x -> x"
    ),
    computeButton
  ),
  div(
    idAttr := "options",
    option("Enable Unsigned Integers (uInt)")
  ),
  div(
    idAttr := "editor",
    onInput.mapToValue --> editorVar
  ),
  div(
    idAttr := "output",
    "Output: ",
    child.text <-- outVar.signal,
    computeButton.events(onClick).map(_ => {
      promptVar.now().evaluate(document.querySelector("#editor").textContent)
    }) --> parserExprObserver
  ),
  h3("StdLib"),
  div(
    idAttr := "stdlib",
    libInfo("Unsigned Integers Extension", List(
      "add" -> "Adds two uInts.",
      "succ" -> "Successor of an uInt."
    ))
  ),
  div(
    idAttr := "footer",
    "Made with ScalaJS, Laminar, Scala 3, Parboiled2, Nix and Mill 🤗."
  ),
)

val containerNode = dom.document.querySelector("#app-container")

@main def run =
  render(containerNode, rootElement)

