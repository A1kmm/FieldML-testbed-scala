package framework.valuesource

import fieldml.evaluator.ParameterEvaluator
import fieldml.evaluator.Evaluator
import fieldml.valueType.ValueType

import framework.value.Value
import framework.Context
import framework.EvaluationState

import framework.datastore._

class ParameterEvaluatorValueSource[UserDofs](name : String, valueType : ValueType, dataStore : DataStore[ValueSource[UserDofs]])
    extends ParameterEvaluator(name, valueType, dataStore)
    with ValueSource[UserDofs]
{
    private val indexes = new Array[Int](dataStore.description.indexEvaluators.size)

    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => Value] =
    {
        val indexes = for(
            eval <- dataStore.description.indexEvaluators;
            value <- eval.evaluate(state))
                yield value
        
        if (dataStore.description.indexEvaluators.size != indexes.size)
          None
        else
          // Note: The .get here will fail if someone tries to evaluate an
          // evaluator at an invalid value - should we have two levels of Option,
          // one that fails independent of UserDofs, and one that depends on UserDofs?
          Some(x => dataStore.description(indexes.map(v => v(x).eValue).toArray).get)
    }
}
