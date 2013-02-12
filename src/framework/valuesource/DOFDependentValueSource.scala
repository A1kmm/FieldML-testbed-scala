package framework.valuesource

import fieldml.valueType._
import fieldml.evaluator.Evaluator

import framework.value._
import framework.Context
import framework.EvaluationState

class DOFDependentValueSource[UserDofs](val value : UserDofs => Value, myValueType : ValueType)
    extends Evaluator[ValueSource[UserDofs]]("DOFDependentValueSource()", myValueType)
    with ValueSource[UserDofs]
{
    override def variables = None
    
    override def toString() : String =
    {
        return "DOFDependentValueSource"
    }
    
    private val _value = Some(value)
    
    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => Value] = _value
}
