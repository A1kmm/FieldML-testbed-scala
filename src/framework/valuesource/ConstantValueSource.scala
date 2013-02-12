package framework.valuesource

import fieldml.valueType._
import fieldml.evaluator.Evaluator

import framework.value.Value
import framework.Context
import framework.EvaluationState

class ConstantValueSource[UserDofs](val value : Value)
    extends Evaluator[ValueSource[UserDofs]]("Constant(" + value.toString + ")", value.vType)
    with ValueSource[UserDofs]
{
    override def variables = None
    
    
    override def toString() : String =
    {
        return "(" + value + ")[ConstantValueSource]"
    }
    
    
    private val _value = Some((_ : UserDofs) => value)
    
    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => Value] = _value
}
