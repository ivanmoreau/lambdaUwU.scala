import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*
import com.ivmoreau.lambdaCore.*
import scala.util.Try
import org.scalacheck.Gen

class CoreTest extends AnyFreeSpec with Checkers {
  "Native natural expressions" - {
    "nNats disabled" in {
      check {  (a: Int, b: Int) =>
        (a >= 0 && b >= 0) ==> {
          val extensions: Extensions = Extensions(useNativeNats = false)
          val computed: String = s"add ${a.toString} ${b.toString}".evaluate("")(extensions)
          computed.contains("Invalid input")
        }
      }
    }

    "nNats enabled" in {
      check { (a: Int, b: Int) =>
        (a >= 0 && b >= 0) ==> {
          val extensions: Extensions = Extensions(useNativeNats = true)
          val computed = s"add ${a.toString} ${b.toString}".evaluate("")(extensions)
          val expected = (a + b).toString
          computed == expected
        }
      }
    }
  }

  "Native booleans expressions" - {
    "nBools disabled" in {
      check {  (a: Boolean) =>
        val extensions: Extensions = Extensions(useNativeBools = false)
        val computed: String = s"if ${a.toString} left right".evaluate("")(extensions)
        computed == s"(((if ${a.toString}) left) right)"
      }
    }

    "nBools enabled" in {
      check {  (a: Boolean) =>
        val extensions: Extensions = Extensions(useNativeBools = true)
        val computed: String = s"if ${a.toString} left right".evaluate("")(extensions)
        computed match
          case "left" if a == true => true
          case "right" if a == false => true
          case otherwise => false
      }

    }
  }

  "Native and booleans" - {
    "eq" in {
      check { forAll (Gen.zip(Gen.choose(0,1000), Gen.choose(0,1000))) { (a: Int, b: Int) =>
          a >= 0 && b >= 0 ==> { 
            val extensions = Extensions(useNativeNats = true, useNativeBools = true)
            val computed = Try { s"eqN ${a.toString} ${b.toString}".evaluate("")(extensions) }.getOrElse("")
            computed == (a == b).toString()
          }
        }
      }
    }
  }

  "Identity applied to identity N times" - {
    "Always reduce to id (untyped)" in {
      check { forAll (Gen.choose(1,1000)) { (a: Int) =>
          val extensions = Extensions()
          val ctx = raw"id := \x -> x."
          val input = List.fill(a)("id").mkString(" ")
          val computed = input.evaluate(ctx)(extensions)
          computed == "id".evaluate(ctx)(extensions)
        } 
      }
    }
  }

  "Typed Lambda" - {

    import Expr.*
    import EType.*
    import EKind.*

    "Can be parsed ~ 0" in {
      val function0 = "#A: *, B: * -> \\x: (A => B) -> x"
      val expected = Lam(Star, Lam(Star, Abs(~+>(TVar(1), TVar(0)), Var(0))))
      val extensions = Extensions(systemFOmega = true)
      val Right(parsed) = ParserExpr(function0, extensions, List("\\")).parseInput.toEither : @unchecked
      true
    }
  }
}
