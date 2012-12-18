package util

import framework.region._

import framework.value.ContinuousValue
import framework.value.Value

import fieldml.evaluator.ArgumentEvaluator
import fieldml.evaluator.Evaluator
import fieldml.valueType.MeshType
import fieldml.valueType.ContinuousType
import scala.util.matching._

/**
 * Very very simplistic FieldML-java to Collada converter.
 */
abstract class MeshExporter
{
    protected val rawXml : String
    protected val openPolygon : String
    protected val closePolygon : String
    protected val wantVolumeMesh : Boolean = false
    protected val startIndicesAt : Int = 0

    protected def fillInTemplate(outputName : String, xyzArray : StringBuilder,
                                 polygonBlock : StringBuilder, polygonCount : Int,
                                 vertexCount : Int, xyzArrayCount : Int ) : String =
    {
        def replacements(m : Regex.Match) : String =
          {
            m.group(1) match {
              case "name"          => outputName
              case "polygonBlock"  => polygonBlock.toString()
              case "polygonCount"  => ("" + polygonCount)
              case "vertexCount"   => ("" + vertexCount)
              case "xyzArray"      => xyzArray.toString()
              case "xyzArrayCount" => ("" + xyzArrayCount)
              case _               => ""
            }
          }
        """\$([A-Z|a-z]+)""".r.replaceAllIn(rawXml, replacements(_))
    }

    protected def formatTriple(value : Option[Value]) : String =
    {
        value match
        {
            case c : Some[ContinuousValue] =>
              "\n" + c.get.value(0) + " " + c.get.value(1) + " " + c.get.value(2);
            case _ => "\n0 0 0"
        }
    }

    protected def formatPair(value : Option[Value], z : Double) : String =
    {
        value match
        {
            case c : Some[ContinuousValue] =>  "\n" + c.get.value(0) + " " + c.get.value(1) + " " + z
            case _ => "\n0 0 0"
        }
    }

    protected def formatPair(value : Option[Value], zValue : Option[Value]) : String =
    {
        value match
        {
            case c : Some[ContinuousValue] => zValue match
            {
                case d : Some[ContinuousValue] => "\n" + c.get.value(0) + " " + c.get.value(1) + " " + d.get.value(0)
                case _ => "\n0 0 0"
            }
            case _ => "\n0 0 0"
        }
    }

    protected def formatSingle( value : Option[Value], y : Double, z : Double) : String =
    {
        value match
        {
            case c : Some[ContinuousValue] =>  "\n" + c.get.value(0) + " " + y + " " + z;
            case _ => "\n0 0 0"
        }
    }

    def export3DFromFieldML(
      outputName : String,
      region : Region, discretisation : Int,
      meshName : String, evaluatorName : String
    ) : String =
    {
        val meshVariable : ArgumentEvaluator = region.getObject( meshName )
        val meshType = meshVariable.valueType.asInstanceOf[MeshType]
        val meshEvaluator : Evaluator = region.getObject( evaluatorName )
        val elementCount = meshType.elementType.elementCount

        val xyzArray = new StringBuilder()
        val polygonBlock = new StringBuilder()
        for( elementNumber <- 1 to elementCount )
        {
            for( i <- 0 to discretisation )
            {
                for( j <- 0 to discretisation )
                {
                    val xi1 : Double = i * 1.0 / discretisation
                    val xi2 : Double = j * 1.0 / discretisation
                    
                    region.bind(meshVariable, elementNumber, xi1, xi2, 0)
                    
                    val value = region.evaluate(meshEvaluator)
                    xyzArray.append(formatTriple(value))

                    region.bind(meshVariable, elementNumber, xi1, xi2, 1)
                    
                    val value2 = region.evaluate(meshEvaluator)
                    xyzArray.append(formatTriple(value2))
                }
            }
            xyzArray.append("\n")

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
                    polygonBlock.append(" " + (startIndicesAt + nodeAtLowerXi1LowerXi2*2))
                    polygonBlock.append(" " + (startIndicesAt + nodeAtLowerXi1UpperXi2*2))
                    if (wantVolumeMesh)
                    {
                      polygonBlock.append(" " + (startIndicesAt + nodeAtUpperXi1LowerXi2*2))
                      polygonBlock.append(" " + (startIndicesAt + nodeAtUpperXi1UpperXi2*2))
                    }
                    else
                    {
                      polygonBlock.append(" " + (startIndicesAt + nodeAtUpperXi1UpperXi2*2))
                      polygonBlock.append(" " + (startIndicesAt + nodeAtUpperXi1LowerXi2*2))
                      polygonBlock.append(closePolygon)
                      polygonBlock.append(openPolygon)
                    }
                    polygonBlock.append(" " + (startIndicesAt + nodeAtLowerXi1LowerXi2*2 + 1))
                    polygonBlock.append(" " + (startIndicesAt + nodeAtLowerXi1UpperXi2*2 + 1))

                    if (wantVolumeMesh)
                    {
                      polygonBlock.append(" " + (startIndicesAt + nodeAtUpperXi1LowerXi2*2 + 1))
                      polygonBlock.append(" " + (startIndicesAt + nodeAtUpperXi1UpperXi2*2 + 1))
                    }
                    else
                    {
                      polygonBlock.append(" " + (startIndicesAt + nodeAtUpperXi1UpperXi2*2 + 1))
                      polygonBlock.append(" " + (startIndicesAt + nodeAtUpperXi1LowerXi2*2 + 1))
                    }
                    polygonBlock.append(closePolygon)
                }
            }
        }

        val polygonCount = discretisation * discretisation * elementCount
        val vertexCount = ( discretisation + 1 ) * ( discretisation + 1 ) * elementCount
        val xyzArrayCount = vertexCount * 3

        fillInTemplate(outputName, xyzArray, polygonBlock,
                       polygonCount * (if (wantVolumeMesh) 1 else 2),
                       vertexCount*2, xyzArrayCount*2)
    }

    def export3DFromFieldMLBind2Meshes(
          outputName : String,
          region : Region, discretisation : Int, evaluatorName : String,
          mesh1Name : String, mesh2Name : String ) : String =
    {
        val mesh1Variable : ArgumentEvaluator = region.getObject( mesh1Name )
        val mesh2Variable : ArgumentEvaluator = region.getObject( mesh2Name )
        val meshEvaluator : Evaluator = region.getObject( evaluatorName )
        val mesh1Type = mesh1Variable.valueType.asInstanceOf[MeshType]
        val mesh2Type = mesh2Variable.valueType.asInstanceOf[MeshType]
        val element1Count = mesh1Type.elementType.elementCount
        val element2Count = mesh2Type.elementType.elementCount

        val xyzArray = new StringBuilder()
        val polygonBlock = new StringBuilder()
        var elementNumber = 0
        for( element1Number <- 1 to element1Count )
        {
            for( element2Number <- 1 to element2Count )
            {
                for( i <- 0 to discretisation )
                {
                    for( j <- 0 to discretisation )
                    {
                        val xi1 : Double = i * 1.0 / discretisation
                        val xi2 : Double = j * 1.0 / discretisation

                        region.bind(mesh1Variable, element1Number, xi1)
                        region.bind(mesh2Variable, element2Number, xi2)

                        val value = region.evaluate(meshEvaluator)
                        xyzArray.append(formatTriple(value))
                    }
                }
                xyzArray.append( "\n" )

                elementNumber = elementNumber + 1

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
                        polygonBlock.append( " " + (startIndicesAt + nodeAtLowerXi1LowerXi2))
                        polygonBlock.append( " " + (startIndicesAt + nodeAtLowerXi1UpperXi2))
                        polygonBlock.append( " " + (startIndicesAt + nodeAtUpperXi1UpperXi2))
                        polygonBlock.append( " " + (startIndicesAt + nodeAtUpperXi1LowerXi2))
                        polygonBlock.append(closePolygon)
                    }
                }
            }
        }

        val polygonCount = discretisation * discretisation * elementNumber
        val vertexCount = ( discretisation + 1 ) * ( discretisation + 1 ) * elementNumber
        val xyzArrayCount = vertexCount * 3

        fillInTemplate(outputName, xyzArray, polygonBlock, polygonCount, vertexCount, xyzArrayCount)
    }
}
