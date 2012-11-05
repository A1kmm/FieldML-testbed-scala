package util.library

import scala.collection.mutable.Map

import fieldml.evaluator._
import fieldml.valueType._

import fieldml.jni.FieldmlApi._
import fieldml.jni.FieldmlApiConstants._
import fieldml.jni.FieldmlDataDescriptionType._
import fieldml.jni.FieldmlDataSourceType._
import fieldml.jni.FieldmlHandleType._
import fieldml.jni.FieldmlHandleType

import framework.valuesource._
import framework.region._
import framework.io.serialize.Deserializer

import util.exception._
import util._

object ExternalEvaluatorGenerator
{
    def generateExternalEvaluator( source : Deserializer, objectHandle : Int ) :
        Evaluator =
    {
        val name = Fieldml_GetObjectDeclaredName( source.fmlHandle, objectHandle )
        val objectType = Fieldml_GetObjectType( source.fmlHandle, objectHandle )
        
        if( objectType != FHT_EXTERNAL_EVALUATOR )
        {
            Fieldml_GetLastError( source.fmlHandle ) match
            {
                case FML_ERR_UNKNOWN_OBJECT => throw new FmlUnknownObjectException( "Object handle " + objectHandle + " is invalid" )
                case _ => throw new FmlTypeException( name, objectType, FHT_EXTERNAL_EVALUATOR )
            }
        }

        name match
        {
            case "interpolator.1d.unit.cubicHermite" => generateContinuousEvaluator( source, objectHandle, name )
            case "interpolator.2d.unit.bicubicHermite" => generateContinuousEvaluator( source, objectHandle, name )
            case "interpolator.3d.unit.tricubicHermite" => generateContinuousEvaluator( source, objectHandle, name )
            case "interpolator.1d.unit.linearLagrange" => generateContinuousEvaluator( source, objectHandle, name )
            case "interpolator.2d.unit.bilinearLagrange" => generateContinuousEvaluator( source, objectHandle, name )
            case "interpolator.3d.unit.trilinearLagrange" => generateContinuousEvaluator( source, objectHandle, name )
            case "interpolator.1d.unit.quadraticLagrange" => generateContinuousEvaluator( source, objectHandle, name )
            case "interpolator.2d.unit.biquadraticLagrange" => generateContinuousEvaluator( source, objectHandle, name )
            case "interpolator.3d.unit.triquadraticLagrange" => generateContinuousEvaluator( source, objectHandle, name )
            case "interpolator.2d.unit.bilinearSimplex" => generateContinuousEvaluator( source, objectHandle, name )
            case "refinement.square.2x2.xi" => generateContinuousEvaluator( source, objectHandle, name )
            case "shape.unit.line" => generateBooleanEvaluator( source, objectHandle, name )
            case "shape.unit.square" => generateBooleanEvaluator( source, objectHandle, name )
            case "shape.unit.cube" => generateBooleanEvaluator( source, objectHandle, name )
            case "shape.unit.triangle" => generateBooleanEvaluator( source, objectHandle, name )
            case "refinement.square.2x2.element" => generateEnsembleEvaluator( source, objectHandle, name )
            case "random.0d.equiprobable.tag" => generateEnsembleEvaluator( source, objectHandle, name )
            case _ => System.err.println( "Unknown external evaluator " + name ); return null
        }
    }
    
    
    private def generateContinuousEvaluator( source : Deserializer, objectHandle : Int, name : String ) :
        Evaluator =
    {
        val evaluatorType : ContinuousType = source.getContinuousType( Fieldml_GetValueType( source.fmlHandle, objectHandle ) )
        val xiNames = Array[String](
            null,
            "chart.1d.argument",
            "chart.2d.argument",
            "chart.3d.argument"
            )
        val linearParamNames = Array[String](
            null,
            "parameters.1d.unit.linearLagrange.argument",
            "parameters.2d.unit.bilinearLagrange.argument",
            "parameters.3d.unit.trilinearLagrange.argument"
            )
        val quadraticParamNames = Array[String](
            null,
            "parameters.1d.unit.quadraticLagrange.argument",
            "parameters.2d.unit.biquadraticLagrange.argument",
            "parameters.3d.unit.triquadraticLagrange.argument",
            null
            )
        val cubicHermiteParamNames = Array[String](
            null,
            "parameters.1d.unit.cubicHermite.argument",
            "parameters.2d.unit.bicubicHermite.argument",
            "parameters.3d.unit.tricubicHermite.argument",
            null
            )

        val linearSimplexParamNames = Array[String](
            null,
            null, //NYI
            "parameters.2d.unit.bilinearSimplex.argument",
            null //NYI
            )
        
        val booleanName = "boolean"


        /** e.g. for Hermite bases, the following Haskell code will compute the correct reordering...
         * let bases = [[(0,0)],[(1,0)],[(0,1)],[(1,1)]]
         * let outer x y = concatMap (\x' -> map (++x') x) y
         * let derivOrder 0 = [[]]; derivOrder n = (derivOrder (n-1)) ++ map ((n-1):) (derivOrder (n-1))
         * let pointOrder' 0 = [[]]; pointOrder' n = (map (0:) (pointOrder' (n-1))) ++ (map (1:) (pointOrder' (n-1)))
         * let pointOrder n = map reverse (pointOrder' n)
         * let targetOrder n = [(p, d) | p <- pointOrder n, d <- derivOrder n]
         * let targetToBasis (pt, d) = map (\i -> (if i `elem` d then 1 else 0, pt!!i)) [0..(length pt - 1)]
         * let targetBasisOrder n = map targetToBasis (targetOrder n)
         * let tensorProdOrder n = foldl' (\s _ -> bases `outer` s) [[]] [0..(n-1)]
         * let computePermutation from to = map (\tov -> fromJust $ tov `elemIndex` from) to
         * let permArray n = computePermutation (targetBasisOrder n) (tensorProdOrder n)
         */
        def reorder(indices : Array[Int])(params : Array[Double]) =
          (indices map ( (index : Int) => params(index)))
        val fparams : ((Array[Double], Array[Double]) => Array[Double], String, String,
                       (Array[Double]) => Array[Double]) =
        name match
        {
            case "interpolator.1d.unit.cubicHermite" => ( new CubicHermite( 1 ).evaluate _, xiNames( 1 ), cubicHermiteParamNames( 1 ), 
                                                          identity (_ : Array[Double])
                                                        )
            case "interpolator.2d.unit.bicubicHermite" => ( new CubicHermite( 2 ).evaluate _, xiNames( 2 ), cubicHermiteParamNames( 2 ),
                                                            reorder(Array(0,1,4,5,2,3,6,7,8,9,12,13,10,11,14,15)))
            case "interpolator.3d.unit.tricubicHermite" => ( new CubicHermite( 3 ).evaluate _, xiNames( 3 ), cubicHermiteParamNames( 3 ),
                                                            reorder(Array(0,1,8,9,2,3,10,11,16,17,24,25,18,19,26,27,4,5,12,13,6,7,14,15,20,21,
                                                                          28,29,22,23,30,31,32,33,40,41,34,35,42,43,48,49,56,57,50,51,58,59,36,
                                                                          37,44,45,38,39,46,47,52,53,60,61,54,55,62,63)))
            case "interpolator.1d.unit.linearLagrange" => ( new LinearLagrange( 1 ).evaluate _, xiNames( 1 ), linearParamNames( 1 ),
                                                           identity (_ : Array[Double]))
            case "interpolator.2d.unit.bilinearLagrange" => ( new LinearLagrange( 2 ).evaluate _, xiNames( 2 ), linearParamNames( 2 ),
                                                              identity (_ : Array[Double]))
            case "interpolator.3d.unit.trilinearLagrange" => ( new LinearLagrange( 3 ).evaluate _, xiNames( 3 ), linearParamNames( 3 ),
                                                               identity (_ : Array[Double]))
            case "interpolator.1d.unit.quadraticLagrange" => ( new QuadraticLagrange( 1 ).evaluate _, xiNames( 1 ), quadraticParamNames( 1 ),
                                                               identity (_ : Array[Double]))
            case "interpolator.2d.unit.biquadraticLagrange" => ( new QuadraticLagrange( 2 ).evaluate _, xiNames( 2 ), quadraticParamNames( 2 ),
                                                                 identity (_ : Array[Double]))
            case "interpolator.3d.unit.triquadraticLagrange" => ( new QuadraticLagrange( 3 ).evaluate _, xiNames( 3 ), quadraticParamNames( 3 ),
                                                                 identity (_ : Array[Double]))
            case "interpolator.2d.unit.bilinearSimplex" => ( new BilinearSimplex().evaluate _, xiNames( 2 ), linearSimplexParamNames( 2 ),
                                                             identity (_ : Array[Double]))
            case "refinement.square.2x2.xi" => ( new GridRefinementXi(Array( 2, 2 ) ).evaluate _, xiNames( 2 ), xiNames( 2 ),
                                                 identity (_ : Array[Double]))
            case _ => System.err.println( "Unknown external evaluator " + name ); return null
        }
        
        val xiHandle = Fieldml_GetObjectByDeclaredName( source.fmlHandle, fparams._2 )
        if( Fieldml_GetObjectName( source.fmlHandle, xiHandle ) == null )
        {
            throw new FmlInvalidObjectException( "Object " + fparams._2 + " needed by " + name + " is not local" )            
        }
        val xiVariable = source.getArgumentOrSubtypeEvaluator( xiHandle )
        
        val phiHandle = Fieldml_GetObjectByDeclaredName( source.fmlHandle, fparams._3 )
        if( Fieldml_GetObjectName( source.fmlHandle, phiHandle ) == null )
        {
            throw new FmlInvalidObjectException( "Object " + fparams._3 + " needed by " + name + " is not local" )            
        }
        val phiVariable = source.getArgumentOrSubtypeEvaluator( phiHandle )
        
        val localName = Fieldml_GetObjectName( source.fmlHandle, objectHandle )
    
        return new ContinuousFunctionEvaluatorValueSource(
          localName, (xi : Array[Double], params : Array[Double]) => fparams._1(xi, fparams._4(params)),
          xiVariable, phiVariable, evaluatorType )
    }
    
    
    private def generateBooleanEvaluator( source : Deserializer, objectHandle : Int, name : String ) :
        Evaluator =
    {
        val evaluatorType : BooleanType = source.getBooleanType( Fieldml_GetValueType( source.fmlHandle, objectHandle ) )
        val xiNames = Array[String](
            null,
            "chart.1d.argument",
            "chart.2d.argument",
            "chart.3d.argument"
            )

        val fparams =
        name match
        {
            case "shape.unit.triangle" => ( new Shape2DUnitTriangle().evaluate _, xiNames( 2 ) )
            case "shape.unit.line" => ( new ShapeUnitLinear( 1 ).evaluate _, xiNames( 1 ) )
            case "shape.unit.square" => ( new ShapeUnitLinear( 2 ).evaluate _, xiNames( 2 ) )
            case "shape.unit.cube" => ( new ShapeUnitLinear( 3 ).evaluate _, xiNames( 3 ) )
            case _ => System.err.println( "Unknown external evaluator " + name ); return null
        }
        
        val xiHandle = Fieldml_GetObjectByDeclaredName( source.fmlHandle, fparams._2 )
        if( Fieldml_GetObjectName( source.fmlHandle, xiHandle ) == null )
        {
            throw new FmlInvalidObjectException( "Object " + fparams._2 + " needed by " + name + " is not local" )
        }
        val xiVariable = source.getArgumentOrSubtypeEvaluator( xiHandle )
        
        val localName = Fieldml_GetObjectName( source.fmlHandle, objectHandle )
    
        return new BooleanFunctionEvaluatorValueSource( localName, fparams._1, xiVariable, evaluatorType )
    }

    private def generateEnsembleEvaluator( source : Deserializer, objectHandle : Int, name : String ) :
        Evaluator =
    {
        val evaluatorType : EnsembleType = source.getEnsembleType( Fieldml_GetValueType( source.fmlHandle, objectHandle ) )
        
            if( name == "random.0d.equiprobable.tag" )
            {
                val evt : ArgumentEvaluator = source.getArgumentEvaluator( Fieldml_GetObjectByDeclaredName( source.fmlHandle,
                                                                                       "random.0d.equiprobable.argument" ) )
                                                 .asInstanceOf[ArgumentEvaluator]
                return new RandomEvaluator( evt, evaluatorType )
            }

            val xiNames = Array[String](
                null,
                "chart.1d.argument",
                "chart.2d.argument",
                "chart.3d.argument"
                )

            val fparams =
            name match
            {
                case "refinement.square.2x2.element" => ( new GridRefinementElement( Array( 2, 2 ) ).evaluate _, xiNames( 2 ) )
                case _ => System.err.println( "Unknown external evaluator " + name ); return null
            }
            
            val argHandle = Fieldml_GetObjectByDeclaredName( source.fmlHandle, fparams._2 )
            if( Fieldml_GetObjectName( source.fmlHandle, argHandle ) == null )
            {
                throw new FmlInvalidObjectException( "Object " + fparams._2 + " needed by " + name + " is not local" )            
            }
            val xiVariable = source.getArgumentOrSubtypeEvaluator( argHandle )
            
            val localName = Fieldml_GetObjectName( source.fmlHandle, objectHandle )
        
            return new EnsembleFunctionEvaluatorValueSource( localName, fparams._1, xiVariable, evaluatorType )
    }
}
