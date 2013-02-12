package framework.valuesource

import fieldml.evaluator.Evaluator
import fieldml.valueType.ValueType
import fieldml.valueType.ContinuousType

import framework.value.ContinuousValue
import framework.ParamSwizzle
import framework.EvaluationState

class ParamSwizzleValueSource[UserDofs](name : String, valueType : ValueType, source : ValueSource[UserDofs], swizzle : Array[Int])
    extends ParamSwizzle(name, valueType, source, swizzle)
    with ValueSource[UserDofs]
{
    private val buffer = new Array[Double]( swizzle.size )
    
    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => ContinuousValue] =
      source.evaluate(state).map(rawVal => (x : UserDofs) => {
          for (i <- 0 until swizzle.size)
            buffer(i) = rawVal(x).cValue(swizzle(i) - 1)
          new ContinuousValue(valueType.asInstanceOf[ContinuousType], buffer)
        })
}
