package fieldml.evaluator

import scala.collection.mutable.Map

import fieldml.valueType.ValueType
import fieldml.valueType.StructuredType
import fieldml.FieldmlObject

abstract class SubtypeEvaluator[EvType <: Evaluator[EvType]] private( val baseEvaluator : EvType, valueType : ValueType, val subname : String ) 
    extends Evaluator[EvType]( baseEvaluator.name + "." + subname, valueType )
{
    def this( baseEvaluator : EvType, subname : String )
    {
        this( baseEvaluator, baseEvaluator.valueType.asInstanceOf[StructuredType].subtype( subname ), subname )
    }
    
    
    def variables = baseEvaluator.variables
}
