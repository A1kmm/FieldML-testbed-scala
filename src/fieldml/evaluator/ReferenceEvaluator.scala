package fieldml.evaluator

import scala.collection.mutable.Map

import fieldml.valueType.ValueType
import fieldml.FieldmlObject

abstract class ReferenceEvaluator[EvType <: Evaluator[EvType]](name : String, val refEvaluator : EvType) 
    extends Evaluator[EvType](name, refEvaluator.valueType)
{
    val binds = Map[EvType, EvType]()
    

    def variables = refEvaluator.variables

    
    def bind(_bind : (EvType, EvType))
    {
        binds( _bind._1 ) = _bind._2
    }
}
