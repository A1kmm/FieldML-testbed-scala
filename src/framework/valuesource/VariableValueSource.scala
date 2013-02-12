package framework.valuesource

import fieldml.valueType._
import fieldml.evaluator.Evaluator

import framework.value.Value
import framework.Context
import framework.EvaluationState

/**
 * Implements a client-side variable. Basically the same as ConstantValueSource, only mutable.
 */
class VariableValueSource[UserDofs](name : String, valueType : ValueType)
    extends Evaluator[ValueSource[UserDofs]](name, valueType)
    with ValueSource[UserDofs]
{
    override def variables = None
    var value : Option[Value] = None
    
    override def toString() : String =
    {
        return "(" + name + ")[VariableValueSource]"
    }
    
    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => Value] =
      value.map(v => (_ : UserDofs) => v)
}
