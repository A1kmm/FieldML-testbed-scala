package framework.valuesource

import scala.collection.mutable.Stack

import fieldml.valueType.ValueType
import fieldml.evaluator.Evaluator
import fieldml.evaluator.SubtypeEvaluator

import framework.value.Value
import framework.value.EnsembleValue
import framework.value.ContinuousValue
import framework.value.StructuredValue
import framework.Context
import framework.EvaluationState

class SubtypeEvaluatorValueSource[UserDofs](baseEvaluator : ValueSource[UserDofs], subname : String)
    extends SubtypeEvaluator[ValueSource[UserDofs]](baseEvaluator, subname)
    with ValueSource[UserDofs]
{
    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => Value] =
      baseEvaluator.evaluate(state).map(m => (x : UserDofs) => m(x).subvalue(subname))
}
