package framework.valuesource

import fieldml.evaluator.Evaluator
import fieldml.valueType.ContinuousType

import framework.value.Value
import framework.ContinuousFunctionEvaluator
import framework.EvaluationState

class ContinuousFunctionEvaluatorValueSource[UserDofs](
  name : String,
  function : (Array[Double], Array[Double]) => Array[Double],
  var1 : ValueSource[UserDofs], var2 : ValueSource[UserDofs], valueType : ContinuousType)
    extends ContinuousFunctionEvaluator(name, function, var1, var2, valueType)
    with ValueSource[UserDofs]
{
}
