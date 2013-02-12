package fieldml.evaluator

import scala.collection.mutable.Map

import fieldml.valueType.ValueType
import fieldml.FieldmlObject

abstract class ConstantEvaluator[EvalType <: Evaluator[EvalType]](name : String, val valueString : String, valueType : ValueType) 
    extends Evaluator[EvalType](name, valueType)
{
    def variables = None
}
