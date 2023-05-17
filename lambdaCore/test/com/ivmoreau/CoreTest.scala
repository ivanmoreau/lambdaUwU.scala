import cats.implicits.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*
import com.ivmoreau.lambdaCore.*

import org.scalatest.matchers.should.Matchers._

import scala.util.Try
import org.scalacheck.Gen
import java.lang.Exception

class CoreTest extends AnyFreeSpec with Checkers {
  "Native natural expressions" - {
    "nNats disabled" in {
      check {
        forAll(Gen.zip(Gen.posNum[Int], Gen.posNum[Int])) { (a: Int, b: Int) =>
          (a >= 0 && b >= 0) ==> {
            val extensions: Extensions = Extensions(useNativeNats = false)
            val computed: String =
              s"add ${a.toString} ${b.toString}".evaluate("")(extensions)
            computed.contains("Parse error: Error(")
          }
        }
      }
    }

    "nNats enabled" in {
      check {
        forAll(Gen.zip(Gen.posNum[Int], Gen.posNum[Int])) { (a: Int, b: Int) =>
          (a >= 0 && b >= 0) ==> {
            val extensions: Extensions = Extensions(useNativeNats = true)
            val computed =
              s"add ${a.toString} ${b.toString}".evaluate("")(extensions)
            val expected = (a + b).toString
            computed == expected
          }
        }
      }
    }
  }

  "Native booleans expressions" - {
    "nBools disabled" in {
      check { (a: Boolean) =>
        val extensions: Extensions = Extensions(useNativeBools = false)
        val computed: String =
          s"if ${a.toString} left right".evaluate("")(extensions)
        computed == s"(((if ${a.toString}) left) right)"
      }
    }

    "nBools enabled" in {
      check { (a: Boolean) =>
        val extensions: Extensions = Extensions(useNativeBools = true)
        val computed: String =
          s"if ${a.toString} left right".evaluate("")(extensions)
        computed match
          case "left" if a == true   => true
          case "right" if a == false => true
          case otherwise             => false
      }

    }
  }

  "Native and booleans" - {
    "eq" in {
      check {
        forAll(Gen.zip(Gen.choose(0, 1000), Gen.choose(0, 1000))) {
          (a: Int, b: Int) =>
            a >= 0 && b >= 0 ==> {
              val extensions =
                Extensions(useNativeNats = true, useNativeBools = true)
              val computed = Try {
                s"eqN ${a.toString} ${b.toString}".evaluate("")(extensions)
              }.getOrElse("")
              computed == (a == b).toString()
            }
        }
      }
    }
  }

  "Identity applied to identity N times" - {
    "Always reduce to id (untyped)" in {
      check {
        forAll(Gen.choose(1, 1000)) { (a: Int) =>
          val extensions = Extensions()
          val ctx = raw"id := \x -> x."
          val input = List.fill(a)("id").mkString(" ")
          val computed = input.evaluate(ctx)(extensions)
          computed == "id".evaluate(ctx)(extensions)
        }
      }
    }
  }

  "Function test" - {
    "curried" in {
      import com.ivmoreau.lambdaCore.Expr.*

      val extensions = Extensions()
      val input = raw"\x -> \y -> \z -> x z (y z)"
      val parsed =
        ParserExpr(input, extensions, List("")).expression.parseAll(input)
      val expected =
        Abs(Abs(Abs(App(App(Var(2), Var(0)), App(Var(1), Var(0))))))
      parsed match
        case Left(err) =>
          throw new java.lang.Exception(s"Error\n${show"$err"}")
        case Right(parsed) =>
          parsed shouldBe expected
    }

    "uncurried" in {
      import com.ivmoreau.lambdaCore.Expr.*

      val extensions = Extensions()
      val input = raw"\x, y, z -> x z (y z)"
      val parsed =
        ParserExpr(input, extensions, List("")).expression.parseAll(input)
      val expected =
        Abs(Abs(Abs(App(App(Var(2), Var(0)), App(Var(1), Var(0))))))
      parsed match
        case Left(err) =>
          throw new java.lang.Exception(s"Error\n${show"$err"}")
        case Right(parsed) =>
          parsed shouldBe expected
    }
  }

}
