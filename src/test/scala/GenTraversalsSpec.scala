import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GenTraversalsSpec extends AnyWordSpec with Matchers {

  final case class Person(name: String) extends Thing

  final case class Animal(name: String, action: String) extends Thing

  @GenTraversals
  sealed trait Thing {
    def name: String
  }

  val things: Seq[Thing] = Seq(Person("joe"), Animal("cat", "meow"))

  "GenTraversals" should {
    "" in {
      val updated = things.map(thing => Thing.name.replace("wow").apply(thing))
      updated shouldBe Seq(Person("wow"), Animal("wow", "meow"))
    }
  }

}
