package framework.valuesource

import fieldml.evaluator.Evaluator
import fieldml.valueType.EnsembleType

import framework.value.Value
import framework.EnsembleFunctionEvaluator
import framework.EvaluationState

class EnsembleFunctionEvaluatorValueSource[UserDofs](name : String, function : (Array[Double]) => Int, var1 : ValueSource[UserDofs],
                                                     valueType : EnsembleType)
    extends EnsembleFunctionEvaluator(name, function, var1, valueType)
    with ValueSource[UserDofs]
{
}
