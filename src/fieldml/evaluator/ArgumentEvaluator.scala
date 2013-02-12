package fieldml.evaluator

import fieldml.valueType.ValueType

abstract class ArgumentEvaluator[EvType <: Evaluator[EvType]]( name : String, valueType : ValueType, explicitVariables : ArgumentEvaluator[EvType]* )
    extends Evaluator[EvType]( name, valueType )
{
    private val _variables = ( ( explicitVariables :+ this ) ++ explicitVariables.flatMap( _.variables ) ).distinct
    
    def variables = _variables
}
