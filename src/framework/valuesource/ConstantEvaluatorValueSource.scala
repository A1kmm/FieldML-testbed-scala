package framework.valuesource

import fieldml.valueType._
import fieldml.evaluator.ConstantEvaluator

import framework.value.Value
import framework.Context
import framework.EvaluationState

class ConstantEvaluatorValueSource[UserDofs](name : String, valueString : String, valueType : ValueType)
    extends ConstantEvaluator[ValueSource[UserDofs]]( name, valueString, valueType )
    with ValueSource[UserDofs]
{
    val value : Value = Value(valueType, valueString)
    
    override def variables = None
    override def toString() : String =
    {
        return "(" + value + ")[ConstantEvaluatorValueSource]"
    }

    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => Value] = Some((_ : UserDofs) => value)
}
