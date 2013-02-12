package framework.datastore

import fieldml.DataSource
import fieldml.evaluator.Evaluator

class DataStore[EvType <: Evaluator[EvType]]( val source : DataSource, val description : DataDescription[EvType] )
{

}
