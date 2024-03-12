object Main {

  import monocle._
  import monocle.macros._
  import monocle.syntax.all._

  @GenTraversals
  sealed trait Thing { def name: String }

  final case class Person(name: String) extends Thing

  final case class Animal(name: String, action: String) extends Thing

  val things: Seq[Thing] = Seq(Person("joe"), Animal("cat", "meow"))

  def main(args: Array[String]): Unit = {
    val modified = things.map(thing => Thing.name.replace("wow").apply(thing))

    println(modified)
  }

}
