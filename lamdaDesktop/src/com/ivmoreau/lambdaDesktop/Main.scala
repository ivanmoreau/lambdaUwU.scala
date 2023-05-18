package com.ivmoreau.lambdaDesktop

import cats.effect.IO
import cats.effect.*
import cats.effect.std.Console
import fs2.io.file.{Files, Path}
import com.ivmoreau.lambdaCore.{evaluate, Extensions}
import cats.data.State
import cats.data.StateT

enum Action:
  case Quit()
  case Booleans()
  case Naturals()
  case Eval(s: String)
  case Load(s: String) // Path to file

case class Data(extensions: Extensions, ctx: Option[String])

def handleInput(input: String): Action =
  input match
    case ":q" | ":quit"           => Action.Quit()
    case ":b" | ":booleans"       => Action.Booleans()
    case ":n" | ":naturals"       => Action.Naturals()
    case i if i.startsWith(":l ") => Action.Load(i.drop(3))
    case i                        => Action.Eval(i)

def readInput: IO[String] =
  Console[IO].print("λ> ") *>
    Console[IO].readLine

def readFile(path: String): IO[String] =
  Files[IO]
    .readAll(Path(path).toNioPath, 4096)
    .through(fs2.text.utf8Decode)
    .compile
    .string

def eval(input: String, ctx: Option[String], extensions: Extensions): String =
  input.evaluate(ctx.getOrElse(""))(extensions)

def handleInput: IO[Action] = readInput.map(handleInput)

def handleActionData(action: Action): Data => IO[Data] = { state =>
  action match
    case Action.Booleans() =>
      IO(
        state.copy(extensions =
          state.extensions.copy(useNativeBools =
            !state.extensions.useNativeBools
          )
        )
      )
    case Action.Naturals() =>
      IO(
        state.copy(extensions =
          state.extensions.copy(useNativeNats = !state.extensions.useNativeNats)
        )
      )
    case Action.Load(s) =>
      readFile(s).map { content =>
        state.copy(ctx = Some(content))
      }
    case _ => IO(state)
}

def pipeline(data: Data): IO[Unit] = handleInput.flatMap {
  case Action.Quit() => IO.unit
  case Action.Eval(s) => 
    val result = eval(s, data.ctx, data.extensions)
    Console[IO].println(result) *> pipeline(data)
  case action => handleActionData(action)(data).flatMap(pipeline)
}
object Main extends IOApp:
  def run(args: List[String]): IO[ExitCode] =
    pipeline(Data(Extensions(), None)).as(ExitCode.Success)
