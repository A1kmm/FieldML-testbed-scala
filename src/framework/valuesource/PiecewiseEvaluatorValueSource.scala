package framework.valuesource

import fieldml.evaluator.PiecewiseEvaluator
import fieldml.evaluator.Evaluator
import fieldml.valueType._

import framework.value.Value
import framework.Context
import framework.EvaluationState

class PiecewiseEvaluatorValueSource[UserDofs]( name : String, valueType : ValueType, index : ValueSource[UserDofs] )
    extends PiecewiseEvaluator( name, valueType, index )
    with ValueSource[UserDofs]
{
    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => Value] =
    {
      state.pushAndApply(name, binds.toSeq.map((evArg) => (evArg._1, evArg._2)))
      val indexSF = index.evaluate(state)
      val indexValues = index.valueType.asInstanceOf[EnsembleType].elementSet.toArray
      val partsM = Map() ++ (for (idx <- indexValues;
                                  eval <- delegations.get(idx);
                                  v <- eval.evaluate(state)) yield (idx, v))
      
      val indexF = index.evaluate(state)
      state.pop()

      indexSF.map(indexF => (u : UserDofs) => partsM(indexF(u).eValue)(u))
    }

}
