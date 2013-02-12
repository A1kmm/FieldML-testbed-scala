package framework

import fieldml.evaluator.Evaluator
import fieldml.valueType.ValueType
import framework.valuesource.ValueSource

abstract class ParamSwizzle[UserDofs](name : String, valueType : ValueType, val source : ValueSource[UserDofs], val swizzle : Array[Int])
    extends Evaluator[ValueSource[UserDofs]](name, valueType)
{
    def variables = source.variables
}
