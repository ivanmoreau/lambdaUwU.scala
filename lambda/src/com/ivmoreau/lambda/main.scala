package com.ivmoreau.lambda

import org.scalajs.dom
import com.raquo.laminar.api.L.*
import org.scalajs.dom.{console, document}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, JSGlobalScope, JSImport}
import com.ivmoreau.lambdaCore.{Expr, ParserExpr, PrinterExpr}
import com.ivmoreau.lambdaCore.evaluate
import com.ivmoreau.lambdaCore.Extensions

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

def checkbox(variable: Var[Boolean]) =
  val class_ = (a: Boolean) => a match
    case true => "check-select"
    case false => "check-unselect"
  button(
    idAttr := "checkbox",
    cls <-- variable.signal.map(class_),
    onClick.map { _ =>
      !variable.now()
    } --> variable
  )

val useNativeNats: Var[Boolean] = Var(false)
val useNativeBools: Var[Boolean] = Var(false)

val extensions: () => Extensions = () => Extensions(
  useNativeNats = useNativeNats.now(),
  useNativeBools = useNativeBools.now()
)

def option(opt: String, v: Var[Boolean]) =
  span(
    checkbox(v),
    opt,
    cls := "option"
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
    option("Enable Native Naturals (nNat)", useNativeNats),
    option("Enable Native Booleans (nBool)", useNativeBools)
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
      promptVar.now().evaluate(document.querySelector("#editor").textContent)(extensions())
    }) --> parserExprObserver
  ),
  h3("StdLib"),
  div(
    idAttr := "stdlib",
    libInfo("Native (non-lambda) Naturals Extension", List(
      "add" -> "Adds two uInts.",
      "succ" -> "Successor of an uInt.",
      "sub" -> "Subtract the second element to the first one without overflow (0 is the lowest).",
      "eqN" -> "Equivalence of native naturals (Only if Booleans extension is enabled)."
    )),
    libInfo("Native (non-lambda) Booleans Extension", List(
      "true" -> "True.",
      "false" -> "False.",
      "if" -> "If-Else comparison. i.e: «if (x) (y) (z)», that reads as 'if x then y else z'"
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

