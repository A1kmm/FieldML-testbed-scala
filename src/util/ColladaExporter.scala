package util

import framework.region._

import framework.value.ContinuousValue
import framework.value.Value

import fieldml.evaluator.ArgumentEvaluator
import fieldml.evaluator.Evaluator
import framework.value.MeshValue
import framework.valuesource._
import fieldml.valueType.MeshType
import fieldml.valueType.ContinuousType
import scala.util.matching._

/**
 * Very very simplistic FieldML-java to Collada converter.
 */
object ColladaExporter extends MeshExporter
{
    override val startIndicesAt = 0
    override val rawXml = """<?xml version="1.0" encoding="utf-8"?>
<COLLADA version="1.4.0" xmlns="http://www.collada.org/2005/11/COLLADASchema">
    <library_effects>
        <effect id="$name_MaterialEffect" name="$name_MaterialEffect">
            <profile_COMMON>
                <technique sid="blender">
                    <phong>
                        <emission>
                            <color>0.00000 0.00000 0.00000 1</color>
                        </emission>
                        <ambient>
                            <color>0.40000 0.40000 0.40000 1</color>
                        </ambient>
                        <diffuse>
                            <color>0.80000 0.80000 0.80000 1</color>
                        </diffuse>
                        <specular>
                            <color>0.50000 0.50000 0.50000 1</color>
                        </specular>
                        <shininess>
                            <float>12.5</float>
                        </shininess>
                        <reflective>
                            <color>1.00000 1.00000 1.00000 1</color>
                        </reflective>
                        <reflectivity>
                            <float>0.0</float>
                        </reflectivity>
                        <transparent>
                            <color>1 1 1 1</color>
                        </transparent>
                        <transparency>
                            <float>0.0</float>
                        </transparency>
                    </phong>
                </technique>
            </profile_COMMON>
        </effect>
    </library_effects>
    <library_materials>
        <material id="$name_Material" name="$name_Material">
            <instance_effect url="#$name_MaterialEffect"/>
        </material>
    </library_materials>
    <library_geometries>
        <geometry id="$name_Geometry" name="$name_Geometry">
            <mesh>
                <source id="$name_Position">
                    <float_array count="xyzArrayCount" id="$name_Position-array">$xyzArray</float_array>
                    <technique_common>
                        <accessor count="vertexCount" source="#$name_Position-array" stride="3">
                            <param type="float" name="X"></param>
                            <param type="float" name="Y"></param>
                            <param type="float" name="Z"></param>
                        </accessor>
                    </technique_common>
                </source>
                <vertices id="$name_Vertex">
                    <input semantic="POSITION" source="#$name_Position"/>
                </vertices>
                <polygons count="polygonCount" material="$name_Material">
                    <input offset="0" semantic="VERTEX" source="#$name_Vertex"/>
$polygonBlock
                </polygons>
            </mesh>
        </geometry>
    </library_geometries>
    <library_visual_scenes>
        <visual_scene id="$name_Scene" name="$name_Scene">
            <node layer="L1" id="$name_Object" name="$name_Object">
                <translate sid="translate">0.00000 0.00000 0.00000</translate>
                <rotate sid="rotateZ">0 0 1 0.00000</rotate>
                <rotate sid="rotateY">0 1 0 -0.00000</rotate>
                <rotate sid="rotateX">1 0 0 0.00000</rotate>
                <scale sid="scale">1.00000 1.00000 1.00000</scale>
                <instance_geometry url="#$name_Geometry">
                    <bind_material>
                        <technique_common>
                            <instance_material symbol="$name_Material" target="#$name_Material">
                                <bind_vertex_input input_semantic="TEXCOORD" input_set="1" semantic="CHANNEL1"/>
                            </instance_material>
                        </technique_common>
                    </bind_material>
                </instance_geometry>
            </node>
        </visual_scene>
    </library_visual_scenes>
    <scene>
        <instance_visual_scene url="#$name_Scene"/>
    </scene>
</COLLADA>    		
    """
    override val openPolygon = "<p>"
    override val closePolygon = "</p>\n"

    def export2DFromFieldML(
      outputName : String, region : Region[MeshValue], discretisation : Int,
      meshName : String, evaluatorName : String) : String =
    {
        val meshVariable : ArgumentEvaluatorValueSource[MeshValue] = region.getObject( meshName )
        val meshType = meshVariable.valueType.asInstanceOf[MeshType]
        val meshEvaluator : ValueSource[MeshValue] = region.getObject( evaluatorName )
        val elementCount = meshType.elementType.elementCount

        val xyzArray = new StringBuilder()
        val polygonBlock = new StringBuilder()

        region.bind(meshVariable, (x : MeshValue) => x)
        val Some(f : (MeshValue => Value)) = region.evaluate(meshEvaluator)

        for( elementNumber <- 1 to elementCount )
        {
            for( i <- 0 to discretisation )
            {
                for( j <- 0 to discretisation )
                {
                    val xi1 : Double = i * 1.0 / discretisation
                    val xi2 : Double = j * 1.0 / discretisation
                    
                    val value = f(new MeshValue(meshType, elementNumber, xi1, xi2))
                    
                    xyzArray.append(formatTriple(value))
                }
            }
            xyzArray.append( "\n" )

            val nodeOffsetOfElement = ( elementNumber - 1 ) * ( discretisation + 1 ) * ( discretisation + 1 )
            for( i <- 0 until discretisation )
            {
                for( j <- 0 until discretisation )
                {
                    val nodeAtLowerXi1LowerXi2 = nodeOffsetOfElement + ( discretisation + 1 ) * ( i + 0 ) + ( j + 0 )
                    val nodeAtLowerXi1UpperXi2 = nodeOffsetOfElement + ( discretisation + 1 ) * ( i + 0 ) + ( j + 1 )
                    val nodeAtUpperXi1UpperXi2 = nodeOffsetOfElement + ( discretisation + 1 ) * ( i + 1 ) + ( j + 1 )
                    val nodeAtUpperXi1LowerXi2 = nodeOffsetOfElement + ( discretisation + 1 ) * ( i + 1 ) + ( j + 0 )
                    polygonBlock.append(openPolygon)
                    polygonBlock.append( " " + (nodeAtLowerXi1LowerXi2 ))
                    polygonBlock.append( " " + (nodeAtLowerXi1UpperXi2 ))
                    polygonBlock.append( " " + (nodeAtUpperXi1UpperXi2 ))
                    polygonBlock.append( " " + (nodeAtUpperXi1LowerXi2 ))
                    polygonBlock.append(closePolygon)
                }
            }
        }

        val polygonCount = discretisation * discretisation * elementCount
        val vertexCount = ( discretisation + 1 ) * ( discretisation + 1 ) * elementCount
        val xyzArrayCount = vertexCount * 3

        fillInTemplate(outputName, xyzArray, polygonBlock, "cube", polygonCount, 8, vertexCount, xyzArrayCount, "trilinearLagrange", 8)
    }


    def export2DTrisFromFieldML(
      outputName : String, region : Region[MeshValue], discretisation : Int,
      meshName : String, evaluatorName : String) : String =
    {
        val meshVariable : ArgumentEvaluatorValueSource[MeshValue] = region.getObject(meshName)
        val meshType = meshVariable.valueType.asInstanceOf[MeshType]
        val meshEvaluator : ValueSource[MeshValue] = region.getObject(evaluatorName)
        val elementCount = meshType.elementType.elementCount

        region.bind(meshVariable, (x : MeshValue) => x)
        val Some(f : (MeshValue => Value)) = region.evaluate(meshEvaluator)

        //TODO Currently ignores discretisation
        
        val xyzArray = new StringBuilder()
        val polygonBlock = new StringBuilder()
        for (elementNumber <- 1 to elementCount)
        {
            for (i <- 0 to 1)
            {
                for (j <- 0 to 1)
                {
                    val xi1 : Double = i * 1.0
                    val xi2 : Double = j * 1.0
                    if (xi1 + xi2 <= 1.0)
                    {
                        val value = f(new MeshValue(meshType, elementNumber, xi1, xi2))
                        
                        xyzArray.append(formatTriple(value))
                    }
                }
            }
            xyzArray.append("\n")

            val nodeOffsetOfElement = (elementNumber - 1) * 3
            var counter = 0
            for (i <- 0 until 1)
            {
                for (j <- 0 until 1)
                {
                    if (i + j <= 1)
                    {
                        val node1 = nodeOffsetOfElement + counter
                        val node2 = nodeOffsetOfElement + counter + 1
                        val node3 = nodeOffsetOfElement + counter + 2
                        polygonBlock.append(openPolygon)
                        polygonBlock.append( " " + node1)
                        polygonBlock.append( " " + node2)
                        polygonBlock.append( " " + node3)
                        polygonBlock.append(closePolygon)
                        
                        counter = counter + 3
                    }
                }
            }
        }

        val polygonCount = elementCount
        val vertexCount = 3 * elementCount
        val xyzArrayCount = vertexCount * 3

        fillInTemplate(outputName, xyzArray, polygonBlock, "cube", polygonCount, 8, vertexCount, xyzArrayCount, "trilinearLagrange", 8)
    }


    def export2DFromFieldML(
      outputName : String, region : Region[MeshValue], discretisation : Int, meshName : String,
      geometryName : String, valueName : String) : String =
    {
        val meshVariable : ArgumentEvaluatorValueSource[MeshValue] = region.getObject(meshName)
        val meshType = meshVariable.valueType.asInstanceOf[MeshType]
        val meshEvaluator : ValueSource[MeshValue] = region.getObject(geometryName)
        val heightEvaluator : ValueSource[MeshValue] = region.getObject(valueName)
        val elementCount = meshType.elementType.elementCount

        region.bind(meshVariable, (x : MeshValue) => x)
        val Some(f : (MeshValue => Value)) = region.evaluate(meshEvaluator)
        val Some(g : (MeshValue => Value)) = region.evaluate(heightEvaluator)

        val xyzArray = new StringBuilder()
        val polygonBlock = new StringBuilder()
        for (elementNumber <- 1 to elementCount)
        {
            for (i <- 0 to discretisation)
            {
                for (j <- 0 to discretisation)
                {
                    val xi1 : Double = i * 1.0 / discretisation
                    val xi2 : Double = j * 1.0 / discretisation

                    val point = new MeshValue(meshType, elementNumber, xi1, xi2, 0)

                    val value = f(point)
                    val zValue = g(point)
                    
                    xyzArray.append(formatPair(value, zValue))
                }
            }
            xyzArray.append("\n")

            val nodeOffsetOfElement = (elementNumber - 1) * (discretisation + 1) * (discretisation + 1)
            for (i <- 0 until discretisation)
            {
                for (j <- 0 until discretisation)
                {
                    val nodeAtLowerXi1LowerXi2 = nodeOffsetOfElement + (discretisation + 1) * (i + 0) + (j + 0)
                    val nodeAtLowerXi1UpperXi2 = nodeOffsetOfElement + (discretisation + 1) * (i + 0) + (j + 1)
                    val nodeAtUpperXi1UpperXi2 = nodeOffsetOfElement + (discretisation + 1) * (i + 1) + (j + 1)
                    val nodeAtUpperXi1LowerXi2 = nodeOffsetOfElement + (discretisation + 1) * (i + 1) + (j + 0)
                    polygonBlock.append(openPolygon)
                    polygonBlock.append(" " + (nodeAtLowerXi1LowerXi2))
                    polygonBlock.append(" " + (nodeAtLowerXi1UpperXi2))
                    polygonBlock.append(" " + (nodeAtUpperXi1UpperXi2))
                    polygonBlock.append(" " + (nodeAtUpperXi1LowerXi2))
                    polygonBlock.append(closePolygon)
                }
            }
        }

        val polygonCount = discretisation * discretisation * elementCount
        val vertexCount = (discretisation + 1) * (discretisation + 1) * elementCount
        val xyzArrayCount = vertexCount * 3

        fillInTemplate(outputName, xyzArray, polygonBlock, "cube", polygonCount, 8, vertexCount, xyzArrayCount, "trilinearLagrange", 8)
    }

    def export1DFromFieldML(
      outputName : String, region : Region[MeshValue], discretisation : Int, meshName : String,
      evaluatorName : String) : String =
    {
        val meshVariable : ArgumentEvaluatorValueSource[MeshValue] = region.getObject(meshName)
        val meshType = meshVariable.valueType.asInstanceOf[MeshType]
        val meshEvaluator : ValueSource[MeshValue] = region.getObject(evaluatorName)
        val elementCount = meshType.elementType.elementCount
        val deltaX = 1.0 / discretisation
        var x = deltaX
        val dimensions = meshEvaluator.valueType.asInstanceOf[ContinuousType].componentType.elementCount

        region.bind(meshVariable, (x : MeshValue) => x)
        val Some(f) = region.evaluate(meshEvaluator)

        val xyzArray = new StringBuilder()
        val polygonBlock = new StringBuilder()
        for (elementNumber <- 1 to elementCount)
        {
            x -= deltaX
            for (j <- 0 to discretisation)
            {
                val xi1 : Double = j * 1.0 / discretisation
  
                val value = f(new MeshValue(meshType, elementNumber, xi1))
                dimensions match
                {
                    case 1 => { xyzArray.append(formatSingle(value, x, 0.0))
                                xyzArray.append(formatSingle(value, x, 1.0)) }
                    case 2 => { xyzArray.append(formatPair(value, 0.0))
                                xyzArray.append(formatPair(value, 1.0)) }
                    case 3 => { xyzArray.append(formatTriple(value))
                                xyzArray.append(formatTriple(value)) }
                }
                
                x += deltaX
            }
            xyzArray.append("\n")

            val nodeOffsetOfElement = (elementNumber - 1) * (discretisation + 1) * 2

            for (j <- 0 until discretisation)
            {
                val nodeAtLowerXi1LowerXi2 = nodeOffsetOfElement + (j * 2) + 0
                val nodeAtLowerXi1UpperXi2 = nodeOffsetOfElement + (j * 2) + 2
                val nodeAtUpperXi1UpperXi2 = nodeOffsetOfElement + (j * 2) + 3
                val nodeAtUpperXi1LowerXi2 = nodeOffsetOfElement + (j * 2) + 1
                polygonBlock.append(openPolygon)
                polygonBlock.append(" " + nodeAtLowerXi1LowerXi2)
                polygonBlock.append(" " + nodeAtLowerXi1UpperXi2)
                polygonBlock.append(" " + nodeAtUpperXi1UpperXi2)
                polygonBlock.append(" " + nodeAtUpperXi1LowerXi2)
                polygonBlock.append(closePolygon)
            }
        }

        val polygonCount = discretisation * elementCount
        val vertexCount = (discretisation + 1) * 2 * elementCount
        val xyzArrayCount = vertexCount * 3

        fillInTemplate(outputName, xyzArray, polygonBlock, "cube", polygonCount, 8, vertexCount, xyzArrayCount, "trilinearLagrange", 8)
    }
}
