package framework

import fieldml.evaluator._
import fieldml.valueType._

import framework.valuesource._

import value.EnsembleValue

class EnsembleFunctionEvaluator[UserDofs](
  name : String,
  val function : (Array [Double]) => Int,
  val var1 : ValueSource[UserDofs],
  valueType : EnsembleType)
    extends Evaluator[ValueSource[UserDofs]](name, valueType) with ValueSource[UserDofs]
{
    private val _variables = var1.variables.toSeq.distinct
    
    def variables = _variables
    
    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => EnsembleValue] =
      var1.evaluate(state).map(fvar1 => x => new EnsembleValue(valueType, function(fvar1(x).cValue)))
}
