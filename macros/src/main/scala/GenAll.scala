import scala.language.experimental.macros
import scala.reflect.macros.blackbox

private class GenAllImpl(val c: blackbox.Context) {
  import c.universe._

  private def directSubCaseObjects[T: c.WeakTypeTag] = {
    val enumTy = weakTypeOf[T]

    val enumTpe = enumTy.typeSymbol

    // The given type T must be a class and it must be sealed
    if (!enumTpe.isClass || !enumTpe.asClass.isSealed) {
      c.abort(
        c.enclosingPosition,
        "Can only enumerate values of a sealed trait or class."
      )
    }

    val enumClass = enumTpe.asClass
    val enumClassChildren =
      enumClass.knownDirectSubclasses.toList.filter(child =>
        child.isClass && child.isModuleClass
      )

    /** There are some potential issues with `knownDirectSubclasses`. Let's try
      * and alleviate some pain by warning when it turns out to be empty.
      */
    if (enumClassChildren.isEmpty) {
      c.warning(
        c.enclosingPosition,
        s"""
           | Enumeration generation for type $enumClass failed to find any
           | instances. 
    """.stripMargin
      )
    }

    enumClassChildren
  }

  def genAll[T: c.WeakTypeTag]: c.Expr[Seq[T]] = {
    val children =
      directSubCaseObjects[T].map(symbol => q"${symbol.asClass.module}")

    c.Expr[Seq[T]](
      q"Seq(..$children)"
    )
  }
}

object GenAll {

  def apply[T]: Seq[T] = macro GenAllImpl.genAll[T]

}
