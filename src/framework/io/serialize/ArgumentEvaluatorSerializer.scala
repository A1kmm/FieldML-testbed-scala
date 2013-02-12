package framework.io.serialize

import fieldml.evaluator.ArgumentEvaluator
import fieldml.valueType.ValueType

import util.exception._

import fieldml.jni.FieldmlApi._
import fieldml.jni.FieldmlHandleType._
import fieldml.jni.FieldmlApiConstants._

import framework.region.UserRegion

import framework.valuesource.ArgumentEvaluatorValueSource


object ArgumentEvaluatorSerializer
{
    def insert[UserDofs](handle : Int, evaluator : ArgumentEvaluatorValueSource[UserDofs]) : Unit =
    {
        val typeHandle = GetNamedObject(handle, evaluator.valueType.name)
        val objectHandle = Fieldml_CreateArgumentEvaluator(handle, evaluator.name, typeHandle)
    }
    
    def extract[UserDofs](source : Deserializer[UserDofs], objectHandle : Int) : ArgumentEvaluatorValueSource[UserDofs] =
    {
        val name = Fieldml_GetObjectName( source.fmlHandle, objectHandle )
        
        val typeHandle = Fieldml_GetValueType( source.fmlHandle, objectHandle )
        
        val valueType = source.getType( typeHandle )
        
        val args = (1 to Fieldml_GetArgumentCount(source.fmlHandle, objectHandle, 1, 1)).map(
            x=>source.getArgumentEvaluator(Fieldml_GetArgument( source.fmlHandle, objectHandle, x, 1, 1 ))
            )
        
        new ArgumentEvaluatorValueSource[UserDofs](name, valueType, args:_*)
    }
}
