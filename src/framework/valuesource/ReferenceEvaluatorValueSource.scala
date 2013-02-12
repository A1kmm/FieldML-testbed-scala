package framework.valuesource

import fieldml.evaluator.ReferenceEvaluator
import fieldml.evaluator.Evaluator
import fieldml.valueType.ValueType

import framework.value.Value
import framework.Context
import framework.EvaluationState

class ReferenceEvaluatorValueSource[UserDofs]( name : String, refEvaluator : ValueSource[UserDofs], valueType : ValueType ) 
    extends ReferenceEvaluator( name, refEvaluator )
    with GenericValueSource[UserDofs]
{
    override def evaluate( state : EvaluationState[UserDofs] ) : Option[UserDofs => Value] =
    {
      evaluateForType( state, valueType )
    }

    override def evaluateForType( state : EvaluationState[UserDofs],
                                  wantedType : ValueType ) : Option[UserDofs => Value] =
    {
        state.pushAndApply(name, binds.toSeq)
        
        val v = refEvaluator match {
          case genValue : GenericValueSource[UserDofs] =>
            genValue.evaluateForType(state, wantedType)
          case _ =>
            refEvaluator.evaluate(state)
        }
        
        state.pop()
        
        v
    }
}
