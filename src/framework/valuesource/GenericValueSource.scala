package framework.valuesource

import fieldml.valueType.ValueType
import framework.value.Value
import framework.EvaluationState

trait GenericValueSource[UserDofs]
    extends ValueSource[UserDofs]
{
    def evaluateForType( state : EvaluationState[UserDofs], wantedType : ValueType ) :
      Option[UserDofs => Value]
}
