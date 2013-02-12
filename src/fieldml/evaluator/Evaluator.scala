package fieldml.evaluator

import scala.collection.mutable.ArrayBuffer

import fieldml.valueType.ValueType
import fieldml.FieldmlObject

abstract class Evaluator[EvalType <: Evaluator[EvalType]](nname : => String, nvalueType : => ValueType)
    extends FieldmlObject( nname )
{
    lazy val valueType : ValueType = nvalueType

    def variables : Iterable[ArgumentEvaluator[EvalType]]
    
    override def toString() : String =
    {
        return name + "[" + getClass.getSimpleName + "]"
    }

    val asEvalType : EvalType
}
