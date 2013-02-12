package framework

import fieldml.evaluator._
import fieldml.valueType._

import framework.valuesource._
import value.Value
import value.BooleanValue

class BooleanFunctionEvaluator[UserDofs](name : String,
                                         val function : ( Array[Double] ) => Boolean,
                                         val var1 : ValueSource[UserDofs], valueType : BooleanType)
    extends Evaluator[ValueSource[UserDofs]](name, valueType) with ValueSource[UserDofs]
{
    private val _variables = ( var1.variables ).toSeq.distinct
    
    def variables = _variables
    
    def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => Value] =
        var1.evaluate(state).map(dToV => x => BooleanValue(valueType, function(dToV(x).cValue)))
}
