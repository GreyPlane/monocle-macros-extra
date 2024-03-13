import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

object GenAllSpec {
  sealed trait TaskType extends Serializable {
    def underlying: String
  }

  object TaskTypes {

    case object A extends TaskType {
      def underlying: String = "a"
    }

    case object B extends TaskType {

      def underlying: String = "b"
    }

  }

}

class GenAllSpec extends AnyWordSpec with Matchers {

  import GenAllSpec._

  "GenAll" should {
    "generate all cases" in {
      val all = GenAll[TaskType]

      all.length shouldBe 2
    }
  }

}
