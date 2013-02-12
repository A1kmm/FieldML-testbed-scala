package framework.valuesource

import fieldml.evaluator._
import fieldml.valueType._
import scala.util.Random

import framework.value._

import framework.EvaluationState

class RandomEvaluator[UserDofs](val randomArgument : ValueSource[UserDofs], valueType : EnsembleType)
    extends Evaluator[ValueSource[UserDofs]]("random.0d.equiprobable", valueType)
    with ValueSource[UserDofs]
{
    def variables = List()
    
    var number : Option[Int] = None

    def isEventuallyUnbound(state : EvaluationState[UserDofs], arg : ValueSource[UserDofs]) : Boolean =
    {
      val bind : Option[ValueSource[UserDofs]] = state.getBind(arg)
      bind match {
        case Some(a : ArgumentEvaluator[ValueSource[UserDofs]]) => isEventuallyUnbound(state, a)
        case Some(_) => false
        case None => true
      }
    }

    override def evaluate(state : EvaluationState[UserDofs]) : Option[UserDofs => Value] =
    {
      evaluateForType(state, valueType)
    }

    def evaluateForType(state : EvaluationState[UserDofs], forType : ValueType) : Option[UserDofs => Value] =
    {
      if (!isEventuallyUnbound(state, randomArgument)) {
        println("Argument appears bound; random tag is identity function.")
        randomArgument.evaluate(state)
      } else {
        val numv = (number match {
          case None => {
            number = Some(math.abs(Random.nextInt()) % forType.asInstanceOf[EnsembleType].elementCount)
            number.get
          }
          case Some(numv) => numv
        })
        Some((u : UserDofs) => EnsembleValue.apply(forType.asInstanceOf[EnsembleType], numv))
      }
    }
}
