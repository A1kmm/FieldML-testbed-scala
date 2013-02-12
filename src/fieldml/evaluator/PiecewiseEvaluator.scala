package fieldml.evaluator

import scala.collection.mutable.Map 

import util.DefaultingHashMap

import fieldml.valueType.EnsembleType
import fieldml.valueType.ValueType
import fieldml.FieldmlObject

import framework.value.EnsembleValue

abstract class PiecewiseEvaluator[EvType <: Evaluator[EvType]](name : String, valueType : ValueType, val index : EvType)
    extends Evaluator[EvType](name, valueType)
{
    val delegations = new DefaultingHashMap[Int, EvType]()
    
    val binds = Map[EvType, EvType]()
    
    def variables = delegations.values.flatMap( _.variables )

    def bind(_bind : (EvType, EvType))
    {
        binds( _bind._1 ) = _bind._2
    }

    
    def map(pair : (Int, EvType))
    {
        delegations( pair._1 ) = pair._2
    }
    
    
    def setDefault(default : EvType)
    {
        delegations.default = default
    }
}
