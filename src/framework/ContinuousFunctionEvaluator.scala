package framework

import fieldml.evaluator._
import fieldml.valueType._

import framework.valuesource._

import value.ContinuousValue
import value.Value

class ContinuousFunctionEvaluator[UserDofs]
(name : String, val function : ( Array[Double], Array[Double] ) => Array[Double],
 val var1 : ValueSource[UserDofs], var2 : ValueSource[UserDofs], valueType : ContinuousType)
    extends Evaluator[ValueSource[UserDofs]](name, valueType) with ValueSource[UserDofs]
{
    private val _variables = (var1.variables ++ var2.variables).toSeq.distinct
    def variables = _variables
    
    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => Value] =
      var1.evaluate(state).flatMap((fvar1 : UserDofs => Value) =>
        var2.evaluate(state).map((fvar2 : UserDofs => Value) => (x : UserDofs) =>
          new ContinuousValue(valueType, function(fvar1(x).cValue, fvar2(x).cValue))
                                ))
}
