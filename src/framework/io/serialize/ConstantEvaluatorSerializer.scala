package framework.io.serialize

import fieldml.evaluator.ConstantEvaluator
import fieldml.evaluator.Evaluator
import fieldml.valueType.ValueType

import util.exception._

import fieldml.jni.FieldmlApi._
import fieldml.jni.FieldmlHandleType._
import fieldml.jni.FieldmlApiConstants._

import framework.region.UserRegion
import framework.valuesource._


object ConstantEvaluatorSerializer
{
    def insert[UserDofs](handle : Int, evaluator : ConstantEvaluator[ValueSource[UserDofs]]) : Unit =
    {
        val valueHandle = GetNamedObject(handle, evaluator.valueType.name)
        
        var objectHandle = Fieldml_CreateConstantEvaluator(handle, evaluator.name, evaluator.valueString, valueHandle);
    }

    
    def extract[UserDofs](source : Deserializer[UserDofs], objectHandle : Int) :
        ConstantEvaluatorValueSource[UserDofs] = 
    {
        val name = Fieldml_GetObjectName( source.fmlHandle, objectHandle )

        val typeHandle = Fieldml_GetValueType( source.fmlHandle, objectHandle )
        
        val valueString = Fieldml_GetConstantEvaluatorValueString( source.fmlHandle, objectHandle )
        
        val valueType : ValueType = source.getType( typeHandle )
        
        val constantEval = new ConstantEvaluatorValueSource[UserDofs]( name, valueString, valueType )
        
        constantEval
    }
}
