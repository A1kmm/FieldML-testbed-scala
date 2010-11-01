package fieldml.evaluator

import fieldml.valueType.ValueType

class AbstractEvaluator( name : String, valueType : ValueType, explicitVariables : AbstractEvaluator* )
    extends Evaluator( name, valueType )
{
    private val _variables = ( ( explicitVariables :+ this ) ++ explicitVariables.flatMap( _.variables ) ).distinct
    
    def variables = _variables
}