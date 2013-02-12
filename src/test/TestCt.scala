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

object TestCt
{
    def main( argv : Array[String] ) : Unit =
    {
        val regionName = "btex20_comp2"
        val fileName = "input\\btex20_comp2.xml"
        val mesh1TypeName = "x_mesh"
        val mesh1ArgumentName = "x_mesh.argument"
        val mesh2TypeName = "t_mesh"
        val mesh2ArgumentName = "t_mesh.argument"
        val meshCoordinatesName = "btex20_comp2.coordinates"
        val outputName = "collada ct.xml"
        val exporter = ColladaExporter.export3DFromFieldMLBind2Meshes _

        val region = UserRegion.fromFile[(MeshValue, MeshValue)](regionName, fileName)

        val mesh1Type : MeshType = region.getObject(mesh1TypeName)
        val mesh1Argument : ArgumentEvaluatorValueSource[(MeshValue, MeshValue)] = region.getObject(mesh1ArgumentName)

        val mesh2Type : MeshType = region.getObject(mesh2TypeName)
        val mesh2Argument : ArgumentEvaluatorValueSource[(MeshValue, MeshValue)] = region.getObject(mesh2ArgumentName)

        val coordinates : ValueSource[(MeshValue, MeshValue)] = region.getObject(meshCoordinatesName)
        
        val test : ValueSource[(MeshValue, MeshValue)] = region.getObject("btex20_comp2.ct")
        val nodes : ValueSource[(MeshValue, MeshValue)] = region.getObject("t_mesh.line.2_nodes")
        val lNode : ArgumentEvaluatorValueSource[(MeshValue, MeshValue)] = region.getObject("linear.node.argument")
        
        region.bind(mesh1Argument, 80, 0.5)
        region.bind(mesh2Argument, 5, 0.5)
        println("*** aggregate1 = " + region.evaluate(test))

        val colladaXml = exporter(regionName, region, 1, meshCoordinatesName, mesh1ArgumentName, mesh2ArgumentName)
        
        val f = new FileWriter(outputName)
        f.write(colladaXml)
        f.close()
    }
}
