import monocle.Traversal

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

//  private val taskComments = Traversal.applyN(
//    GenPrism[Task, Task.RoomFlipTask].andThen(GenLens[Task.RoomFlipTask](_.comments)),
//    GenPrism[Task, Task.RoomMarkUpTask].andThen(GenLens[Task.RoomMarkUpTask](_.comments)),
//    GenPrism[Task, Task.RerentStartTask].andThen(GenLens[Task.RerentStartTask](_.comments)),
//    GenPrism[Task, Task.HotelRoutineTask].andThen(GenLens[Task.HotelRoutineTask](_.comments)),
//    GenPrism[Task, Task.HotelCheckOutTask].andThen(GenLens[Task.HotelCheckOutTask](_.comments)),
//    GenPrism[Task, Task.ApartmentRoutineTask].andThen(GenLens[Task.ApartmentRoutineTask](_.comments)),
//    GenPrism[Task, Task.ApartmentCheckOutTask].andThen(GenLens[Task.ApartmentCheckOutTask](_.comments))
//  )

class nonGeneric extends StaticAnnotation

private class GenTraversalImpl(val c: blackbox.Context) {
  import c.universe._

  def fieldsOf(tpe: Type): List[(TermName, Type)] = {
    val clazz = tpe.typeSymbol.asClass
    val isCaseClass = clazz.isCaseClass
    // Case class field names have an extra space at the end.
    val nameOf: TermSymbol => TermName =
      if (!isCaseClass) _.name
      else field => TermName(field.name.toString.dropRight(1))
    if (isCaseObjectLike(clazz)) Nil
    else
      tpe.decls.sorted.collect {
        case field: TermSymbol if isCaseAccessorLike(field, isCaseClass) =>
          nameOf(field) -> field.typeSignatureIn(tpe).finalResultType
      }
  }

  def isCaseObjectLike(sym: ClassSymbol): Boolean = sym.isModuleClass

  def isCaseAccessorLike(sym: TermSymbol, inCaseClass: Boolean): Boolean = {
    val isGetter =
      if (inCaseClass) sym.isCaseAccessor && !sym.isMethod
      else sym.isGetter && sym.isPublic && (sym.isParamAccessor || sym.isLazy)
    isGetter && !isNonGeneric(sym)
  }

  def isNonGeneric(sym: Symbol): Boolean = {
    def check(sym: Symbol): Boolean = {
      // See https://issues.scala-lang.org/browse/SI-7424
      sym.typeSignature // force loading method's signature
      sym.annotations.foreach(_.tree.tpe) // force loading all the annotations

      sym.annotations.exists(_.tree.tpe =:= typeOf[nonGeneric])
    }

    // See https://issues.scala-lang.org/browse/SI-7561
    check(sym) ||
    (sym.isTerm && sym.asTerm.isAccessor && check(sym.asTerm.accessed)) ||
    sym.overrides.exists(isNonGeneric)
  }

  private def genPrism(
      traitTpe: Type,
      childTpe: Type,
      field: TermName
  ): Tree = {
    fieldsOf(childTpe).find(_._1 == field) match {
      case Some((field, ty)) => q"""
          GenPrism[$traitTpe, $childTpe].andThen(GenLens[$childTpe](_.$field))
         """
      case None              => c.abort(c.enclosingPosition, "field not found")
    }
  }

  private def directSubcaseclasses[T: c.WeakTypeTag]: List[Symbol] = {
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
    val enumClassChildren = enumClass.knownDirectSubclasses.toList

    /** There are some potential issues with `knownDirectSubclasses`. Let's try
      * and alleviate some pain by warning when it turns out to be empty.
      */
    if (enumClassChildren.isEmpty) {
      c.warning(
        c.enclosingPosition,
        s"""
           | Enumeration generation for type $enumClass failed to find any
           | instances. There are bugs in scala macros that may lead to this:
           |
           |  * If you are generating instances of a sealed class that extends
           |    another class, this does not work; or
           |
           |  * If you assign the generated instances to a `val` instead of a
           |    `def`, then the generated instances will be empty; or
           |
           |  * If the macro call (`sealerate.values[T]`) is made before the
           |    `case object` definitions, the generated instances will be
           |    empty
           |
    """.stripMargin
      )
    }

    enumClassChildren.filter(_.isClass)
  }

  private def genTraversal[S, A](
      traitTpe: Type,
      childrenTpes: Seq[Type],
      field: String
  ): c.Expr[Traversal[S, A]] = {
    val prisms = childrenTpes.map(childTpe =>
      genPrism(traitTpe, childTpe, TermName(field))
    )
    c.Expr[Traversal[S, A]](q"""
    import monocle.Traversal
    import monocle.macros.{GenPrism, GenLens}

    Traversal.applyN(
     ..$prisms
    )
    """)
  }

  def genTraversalImpl[S: c.WeakTypeTag, A](
      field: c.Expr[String]
  ): c.Expr[Traversal[S, A]] = {
    field.tree match {
      case Literal(Constant(str: String)) =>
        val enumTy = weakTypeOf[S]
        val children = directSubcaseclasses[S].map(_.asType.toType)

        genTraversal[S, A](enumTy, children, str)

      case _ => c.abort(c.enclosingPosition, "must be string constant")
    }
  }

}

object GenTraversal {

  def apply[S, A](field: String): Traversal[S, A] =
    macro GenTraversalImpl.genTraversalImpl[S, A]

}
