import monocle.{Traversal, Prism, Lens, Fold, Setter, Getter, Optional}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import monocle.syntax.all._

class GenTraversalSpec extends AnyWordSpec with Matchers {

  sealed trait Task {
    def tag: String
  }

  final case class ATask(tag: String, a: String) extends Task

  final case class BTask(tag: String, b: String) extends Task

  val tag = GenTraversal[Task, String]("tag")

  case class State(tasks: Map[Int, Task])

  val testState = State(Map(1 -> ATask("a", "a"), 2 -> BTask("b", "b")))

  "GenTraversal" should {
    "simplify state modification" in {
      val updated = {
        testState.focus(_.tasks).index(1).andThen(tag).replace("tag")
      }

      updated.tasks(1) shouldBe ATask("tag", "a")
    }
  }

}
