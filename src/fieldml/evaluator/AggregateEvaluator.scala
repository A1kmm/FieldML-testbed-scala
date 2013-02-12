package fieldml.evaluator

import scala.collection.mutable.Map 

import util.DefaultingHashMap

import fieldml.valueType.EnsembleType
import fieldml.valueType.ContinuousType
import fieldml.FieldmlObject

abstract class AggregateEvaluator[EvType <: Evaluator[EvType]]( name : String, valueType : ContinuousType )
    extends Evaluator[EvType](name, valueType)
{
    val componentEvaluators = new DefaultingHashMap[Int, EvType]()
    
    val binds = Map[EvType, EvType]()
    
    //NOTE Currently, continuous types can have no more than one component index
    val indexBinds = Map[Int, EvType]()
    
    def variables = componentEvaluators.values.flatMap( _.variables )

    def bind(_bind : (EvType, EvType))
    {
        binds( _bind._1 ) = _bind._2
    }
    
    
    def bind_index(_bind : (Int, EvType))
    {
        indexBinds( _bind._1 ) = _bind._2
    }

    
    def map(pair : (Int, EvType))
    {
        componentEvaluators( pair._1 ) = pair._2
    }
    
    
    def setDefault(default : EvType)
    {
        componentEvaluators.default = default
    }
}
