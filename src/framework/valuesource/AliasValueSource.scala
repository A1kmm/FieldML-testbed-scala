package framework.valuesource

import scala.collection.mutable.Stack

import fieldml.valueType.ValueType
import fieldml.FieldmlObject

import framework.value.Value
import framework.Context
import framework.EvaluationState

class AliasValueSource( sourceType : ValueType, val target : FieldmlObject )
    extends ValueSource( sourceType )
{
    override def getValue( state : EvaluationState ) : Option[Value] =
    {
        return state.get( target )
    }
    
    
    override def toString() : String =
    {
        return "(" + valueType + " -> " + target + ")[Alias]"
    }
}
