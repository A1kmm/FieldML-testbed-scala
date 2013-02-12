package framework.valuesource

import scala.collection.mutable.Stack

import fieldml.valueType.ValueType
import fieldml.FieldmlObject
import fieldml.evaluator.Evaluator
import fieldml.evaluator.ArgumentEvaluator

import framework.value.Value
import framework.Context
import framework.EvaluationState

class ArgumentEvaluatorValueSource[UserDofs](name : String, valueType : ValueType, val explicitVariables : ArgumentEvaluator[ValueSource[UserDofs]]*)
    extends ArgumentEvaluator(name, valueType, explicitVariables : _*)
    with ValueSource[UserDofs]
{
    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => Value] =
    {
        val argumentBinds = explicitVariables.map(
          x => (x.asEvalType, new DOFDependentValueSource[UserDofs]
                (x.asEvalType.evaluate(state).get, valueType)))
        state.resolve(this, argumentBinds)
    }
}
