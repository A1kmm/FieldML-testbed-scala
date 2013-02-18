package test

import scala.collection.mutable.ArrayBuffer
import java.io.FileWriter
import fieldml._
import fieldml.valueType._
import fieldml.evaluator._
import framework.datastore._
import framework.value._
import framework.valuesource._
import framework._
import fieldml.jni.FieldmlApi._
import util.ColladaExporter
import framework.region._
import valuesource.ParameterEvaluatorValueSource

object TriquadraticReadTest
{
    def main( argv : Array[String] ) : Unit =
    {
        val configStrings = ("input/triquadratic heart test.xml", "heart.mesh.type", "heart.mesh.argument", List("heart.coordinates"))
        
        // configStrings = Tuple4( "input\\StaticAdvectionDiffusion.xml", "static_advection_diffusion.mesh", "static_advection_diffusion.mesh.argument", "static_advection_diffusion.geometric" )
        
        val region = UserRegion.fromFile[MeshValue]( "heart", configStrings._1 )

        val meshType : MeshType = region.getObject( configStrings._2 )
        val meshArgument : ArgumentEvaluatorValueSource[MeshValue] = region.getObject( configStrings._3 )

        val coordinates : ValueSource[MeshValue] = region.getObject( configStrings._4(0) )
        
        val triquadNodes : ParameterEvaluatorValueSource[MeshValue] = region.getObject( "heart.node.coordinates" )
        val nodeArgument : ArgumentEvaluatorValueSource[MeshValue] = region.getObject( "heart.nodes.argument" )
        val rc3Argument : ArgumentEvaluatorValueSource[MeshValue] = region.getObject( "heart.rc3.component" )
        
        region.bind( meshArgument, 1, 0.5, 0.5, 0.5 )

        println( "*** aggregate = " + region.evaluate( coordinates ) )

        val colladaXml = ColladaExporter.export3DFromFieldML("heart", region, 8, configStrings._3, configStrings._4)

//        val colladaXml = ColladaExporter.export2DFromFieldML( region, 1, configStrings._3, configStrings._4, "static_advection_diffusion.dependent" )
        val f = new FileWriter( "collada heart.xml" )
        f.write( colladaXml )
        f.close()
    }
}
