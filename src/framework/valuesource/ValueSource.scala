package framework.valuesource

import fieldml.evaluator.Evaluator

import framework.value.Value
import fieldml.valueType._
import framework.EvaluationState

trait ValueSource[UserDofs]
    extends Evaluator[ValueSource[UserDofs]]
{
    def evaluate( state : EvaluationState[UserDofs]) : Option[UserDofs => Value]

    override val asEvalType : ValueSource[UserDofs] = this
}
