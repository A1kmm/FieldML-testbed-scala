package framework

import scala.collection.immutable.TreeMap
import scala.math.Ordering
import scala.math.Ordering.Implicits

import fieldml._
import fieldml.valueType._
import fieldml.evaluator._

import value._
import valuesource._

import util.exception._

object Context {
    private final def emptyBinds[UserDofs] =
      TreeMap[ValueSource[UserDofs], (ValueSource[UserDofs], Context[UserDofs])]()(new Ordering[ValueSource[UserDofs]] {
      // For some applications, this code is the 'bottleneck' so we write out
      // compare explicitly to avoid any unneeded allocation overheads.
          def compare(e1 : ValueSource[UserDofs], e2 : ValueSource[UserDofs]) : Int = {
            val r = e1.hashCode.compare(e2.hashCode)
            if (r != 0)
              r
            else
              e1.name.compare(e2.name)
          }
        })
}
class Context[UserDofs](
  val location : String,
  parentBinds : Map[ValueSource[UserDofs], (ValueSource[UserDofs], Context[UserDofs])],
  initialBinds : Seq[(ValueSource[UserDofs], ValueSource[UserDofs])])
{
    def this(location : String, otherContext : Option[Context[UserDofs]],
             initialBinds : Seq[(ValueSource[UserDofs], ValueSource[UserDofs])]) =
    {
        this(location,
             otherContext.map(_.binds).getOrElse(Context.emptyBinds[UserDofs]), initialBinds)
    }
    
    private lazy val binds : Map[ValueSource[UserDofs], (ValueSource[UserDofs], Context[UserDofs])] =
      parentBinds ++ initialBinds.map(x => (x._1, (x._2, this)))
    
    def getBind(evaluator : ValueSource[UserDofs]) : Option[ValueSource[UserDofs]] =
      binds.get(evaluator).map(_._1)

    def getBindContext(evaluator : ValueSource[UserDofs]) : Option[Context[UserDofs]] =
        binds.get(evaluator).map(_._2)
}
