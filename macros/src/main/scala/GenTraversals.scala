import scala.reflect.macros.whitebox
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros

@compileTimeOnly("enable macro paradise to expand macro annotations")
class GenTraversals extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro GenTraversalsImpl.genTraversals
}

class GenTraversalsImpl(val c: whitebox.Context) {

  import c.universe._

  def genTraversals(annottees: c.Expr[Any]*): c.Tree = {

    annottees.map(_.tree) match {
      case (t @ q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }") :: Nil =>
        def traversals(paramss: Seq[Tree]): Seq[ValDef] = {
          paramss.collect {
            case q"$mods def $tname[..$tparams](...$paramss): $tpt" =>
              q"val $tname : Traversal[$tpname, $tpt] = GenTraversal[$tpname, $tpt](${tname.toString()})"
          }
        }

        q"""
           $t

           object ${tpname.toTermName} {
            import monocle._
            import monocle.macros._

            ..${traversals(stats)}
           }
         """
    }
  }
}
