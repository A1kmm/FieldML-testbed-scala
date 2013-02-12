package framework

import fieldml.evaluator.ParameterEvaluator
import fieldml.evaluator.Evaluator

package object datastore
{
    implicit def parameterDatastoreDescription[EvType <: Evaluator[EvType]](evaluator : ParameterEvaluator[EvType]) = evaluator.dataStore.description
}
