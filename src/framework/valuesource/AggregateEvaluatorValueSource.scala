package framework.valuesource

import fieldml.evaluator.ArgumentEvaluator
import fieldml.evaluator.AggregateEvaluator
import fieldml.evaluator.Evaluator
import fieldml.valueType.ContinuousType
import fieldml.valueType.EnsembleType

import framework.value.Value
import framework.value.ContinuousValue
import framework.value.EnsembleValue
import framework.Context
import framework.EvaluationState

class AggregateEvaluatorValueSource[UserDofs](name : String, valueType : ContinuousType)
    extends AggregateEvaluator[ValueSource[UserDofs]](name, valueType)
    with ValueSource[UserDofs]
{
    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => Value] =
    {
        val indexType = indexBinds(1).valueType.asInstanceOf[EnsembleType]
        val indexEvaluator = new VariableValueSource[UserDofs](name + ".index", indexType)
        val indexValues = for (i <- 1 to indexType.elementCount) yield new EnsembleValue(indexType, i)
        
        state.pushAndApply(name,
                           binds.toSeq.map
                             (v => (v._1,
                                    v._2.asInstanceOf[ValueSource[UserDofs]])) ++
                           indexBinds.toSeq.map((t : (Int, ValueSource[UserDofs])) =>
                             (t._2, indexEvaluator)))

        val values = for (
          i <- indexValues;
          e <- componentEvaluators.get(i.eValue);
          v <- { indexEvaluator.value = Some(i); e.evaluate(state)}) yield v

        state.pop()

        if (values.size != indexValues.size)
          None
        else
          Some(x => new ContinuousValue(valueType, values.flatMap((v : UserDofs => Value) => v(x).cValue).toArray))
    }
}
