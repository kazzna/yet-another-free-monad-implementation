package yafmi

import org.scalatest.freespec.AnyFreeSpec
import scalaz.std.option.optionInstance

class FreeSpec extends AnyFreeSpec {
  "An Pure Free" - {
    val a: Free[Option, Int] = Free.pure(42)
    "when run called" - {
      "return result" in {
        assert(a.run == Option(42))
      }

      "if it's mapped" - {
        "return  result" in {
          val b = a.map(_.toDouble)
          assert(b.run == Option(42d))
        }
      }

      "if it's flatMapped" - {
        "return result" - {
          val b = a.flatMap[String](_ => Free.liftF(None))
          assert(b.run.isEmpty)
        }
      }

      "if it's applied" - {
        "return result" in {
          val f1 = Free.liftF(Option((i: Int) => i.toDouble))
          val b = a.ap(f1)
          assert(b.run == Option(42d))
        }
      }

      "if it's map2-ed" - {
        "return result" in {
          val b = Free.liftF(Option(3.14))
          val c = a.map2(b)((i, d) => s"$i, $d")

          assert(c.run == Option("42, 3.14"))
        }
      }

      "if it's modified multiple times" - {
        "return result" in {
          val b = a.flatMap(i => Free.liftF(Option(i.toDouble)))
          val f2 = Free.liftF(Option((d: Double) => d.toLong))
          val f3 = Free.liftF(Option((l: Long) => l.toString))
          val c = b.ap(f2).ap(f3)
          assert(c.run == Option("42"))
        }
      }
    }
  }
}
