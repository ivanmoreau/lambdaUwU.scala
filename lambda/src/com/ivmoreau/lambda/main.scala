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
    idAttr := "editor",
    onInput.mapToValue --> editorVar
  ),
  div(
    idAttr := "output",
    "Output: ",
    child.text <-- outVar.signal,
    computeButton.events(onClick).map(_ => {
      println("Caled")
      promptVar.now().evaluate(document.querySelector("#editor").textContent)
    }) --> parserExprObserver
  )
)

val containerNode = dom.document.querySelector("#app-container")

@main def run =
  render(containerNode, rootElement)

