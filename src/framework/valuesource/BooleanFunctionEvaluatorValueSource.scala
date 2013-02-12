package framework.valuesource

import fieldml.evaluator.Evaluator
import fieldml.valueType.BooleanType

import framework.value.Value
import framework.BooleanFunctionEvaluator
import framework.EvaluationState

class BooleanFunctionEvaluatorValueSource[UserDofs](name : String, function : (Array[Double]) => Boolean,
                                                    var1 : ValueSource[UserDofs], valueType : BooleanType)
    extends BooleanFunctionEvaluator(name, function, var1, valueType)
    with ValueSource[UserDofs]
{
}
