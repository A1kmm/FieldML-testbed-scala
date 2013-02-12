package framework.io

import fieldml.valueType.ContinuousType
import fieldml.valueType.EnsembleType
import fieldml.valueType.MeshType
import fieldml.valueType.BooleanType
import fieldml.DataSource
import fieldml.DataResource
import fieldml.evaluator.Evaluator
import fieldml.evaluator.ArgumentEvaluator
import fieldml.evaluator.AggregateEvaluator
import fieldml.evaluator.ConstantEvaluator
import fieldml.evaluator.PiecewiseEvaluator
import fieldml.evaluator.ParameterEvaluator
import fieldml.evaluator.ReferenceEvaluator

import util.exception._

import fieldml.jni.FieldmlApi._
import fieldml.jni.FieldmlApiConstants._

import framework.valuesource.ValueSource

package object serialize
{
    def GetNamedObject( handle : Int, name : String ) : Int =
    {
        val objectHandle = Fieldml_GetObjectByName( handle, name )
        if( objectHandle == FML_INVALID_HANDLE )
        {
            //TODO Use the right region name.
            throw new FmlUnknownObjectException( name, "" )
        }
        
        return objectHandle
    }
    
    
    def GetBinds[UserDofs](source : Deserializer[UserDofs], objectHandle : Int) : Seq[(ValueSource[UserDofs], ValueSource[UserDofs])] =
    {
        val bindCount = Fieldml_GetBindCount( source.fmlHandle, objectHandle )
        
        for( i <- 1 to bindCount;
            variable = source.getArgumentOrSubtypeEvaluator( Fieldml_GetBindArgument( source.fmlHandle, objectHandle, i ) );
            evaluator = source.getEvaluator( Fieldml_GetBindEvaluator( source.fmlHandle, objectHandle, i ) )
            )
            yield Tuple2( variable, evaluator )
    }
    

    implicit def continuousTypeSerializer(valueType : ContinuousType) = ContinuousTypeSerializer
    implicit def ensembleTypeSerializer(valueType : EnsembleType) = EnsembleTypeSerializer
    implicit def meshTypeSerializer(valueType : MeshType) = MeshTypeSerializer
    implicit def booleanTypeSerializer(valueType : BooleanType) = BooleanTypeSerializer
    implicit def dataResourceSerializer(dataResource : DataResource) = DataResourceSerializer
    implicit def argumentEvaluatorSerializer[EvalType <: Evaluator[EvalType]](evaluator : ArgumentEvaluator[EvalType]) = ArgumentEvaluatorSerializer
    implicit def piecewiseEvaluatorSerializer[EvalType <: Evaluator[EvalType]](evaluator : PiecewiseEvaluator[EvalType]) = PiecewiseEvaluatorSerializer
    implicit def parameterEvaluatorSerializer[EvalType <: Evaluator[EvalType]](evaluator : ParameterEvaluator[EvalType]) = ParameterEvaluatorSerializer
    implicit def aggregateEvaluatorSerializer[EvalType <: Evaluator[EvalType]](evaluator : AggregateEvaluator[EvalType]) = AggregateEvaluatorSerializer
    implicit def referenceEvaluatorSerializer[EvalType <: Evaluator[EvalType]](evaluator : ReferenceEvaluator[EvalType]) = ReferenceEvaluatorSerializer
    implicit def constantEvaluatorSerializer[EvalType <: Evaluator[EvalType]](evaluator : ConstantEvaluator[EvalType]) = ConstantEvaluatorSerializer
}
