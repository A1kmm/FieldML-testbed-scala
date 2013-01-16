package util

import framework.region._

import framework.value.ContinuousValue
import framework.value.Value
import framework.valuesource.BooleanFunctionEvaluatorValueSource
import framework.FmlException

import fieldml.evaluator.ArgumentEvaluator
import fieldml.evaluator.Evaluator
import fieldml.valueType.MeshType
import fieldml.valueType.ContinuousType
import scala.util.matching._

/**
 * Convert a FieldML file to some other format by resampling.
 */
abstract class MeshExporter
{
    protected val rawXml : String
    protected val openPolygon : String
    protected val closePolygon : String
    protected val wantVolumeMesh : Boolean = false
    protected val startIndicesAt : Int = 0

    protected def fillInTemplate(outputName : String, xyzArray : StringBuilder,
                                 polygonBlock : StringBuilder, shape : String,
                                 polygonCount : Int, nodeCount : Int,
                                 vertexCount : Int, xyzArrayCount : Int,
                                 interpolator : String) : String =
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
              case "shape"         => shape
              case "interpolator"  => interpolator
              case "nodeCount"     => ("" + nodeCount)
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

    def getLibraryShapeName(meshEvaluator : Evaluator) : (String, Int) =
    {
      if (!meshEvaluator.valueType.isInstanceOf[MeshType])
        throw new FmlException("The 'mesh argument evaluator' " + meshEvaluator.name +
                               " selected by the user does not have a mesh type. This could mean " +
                               "that the user supplied an invalid value for the mesh argument evaluator, or " +
                               "that the argumentEvaluator has the wrong valueType attribute.")
      val meshType : MeshType = meshEvaluator.valueType.asInstanceOf[MeshType]
      if (!meshType.shapes.isInstanceOf[BooleanFunctionEvaluatorValueSource])
        throw new FmlException("The mesh argument evaluator'" + meshEvaluator.name +
                               " has a shape that is not one of the built in boolean " +
                               "evaluators. Unfortunately, this case is not " +
                               "supported yet.")
      val shapeEval : BooleanFunctionEvaluatorValueSource =
        meshType.shapes.asInstanceOf[BooleanFunctionEvaluatorValueSource]

      // TODO: support 1d and 2d cases too...

      // Build a signature from the shape function by evaluating at 3 points.
      // TODO: Save this information rather than reconstructing it like this.
      val sig = List((1, Array(0.0, 1.0, 1.0)), (2, Array(1.0, 0.0, 1.0)), (4, Array(1.0, 1.0, 0.0))).
                 map(weightXis => if (shapeEval.function(weightXis._2)) weightXis._1 else 0).sum
      sig match {
        case 0 => ("shape.unit.tetrahedron", 4)
        case 3 => ("shape.unit.wedge12", 6)
        case 5 => ("shape.unit.wedge13", 6)
        case 6 => ("shape.unit.wedge23", 6)
        case 7 => ("shape.unit.cube", 8)
        case _ => throw new FmlException("The mesh evaluator " + meshEvaluator.name +
                                         " uses a built-in shape function that could not be identified.")
      }
    }

    private def subdivide3D(
        region : Region,
        meshVariable : ArgumentEvaluator,
        meshEvaluator : Evaluator,
        polygonBlock : StringBuilder,
        xyzArray : StringBuilder,
        discretisation : Int,
        elementCount : Int
      ) = {
        for( elementNumber <- 1 to elementCount )
        {
            System.out.println("elementNumber: " + elementNumber)
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
    }

    private def simplifyOnly3D(
        region : Region,
        meshVariable : ArgumentEvaluator,
        meshEvaluator : Evaluator,
        polygonBlock : StringBuilder,
        xyzArray : StringBuilder,
        shape : String,
        elementCount : Int
      ) = {
      var usedNodes = Map[(Double, Double, Double),Int]()
      var nextNodeID = 1
      def idForNode(p : (Double, Double, Double)) : Int =
        if (usedNodes.contains(p))
          usedNodes(p)
        else {
          val newNodeID = nextNodeID
          nextNodeID = nextNodeID + 1
          usedNodes = usedNodes + ((p, newNodeID))
          newNodeID
        }

      val z : Double = 0.0
      val o : Double = 1.0
      val nPoints = shape match {
        case "shape.unit.cube"        => List((z,z,z),(z,z,o),(z,o,z),(z,o,o),
                                              (o,z,z),(o,z,o),(o,o,z),(o,o,o)
                                             )
        case "shape.unit.tetrahedron" => List((z,z,z),(z,z,o),(z,o,z),
                                              (o,z,z)
                                             )
        case "shape.unit.wedge12"     => List((z,z,z),(z,z,o),(z,o,z),(z,o,o),
                                              (o,z,z),(o,z,o)
                                             )
        case "shape.unit.wedge13"     => List((z,z,z),(z,z,o),(z,o,z),(z,o,o),
                                              (o,z,z),(o,o,z)
                                             )
        case "shape.unit.wedge23"     => List((z,z,z),(z,z,o),(z,o,z),
                                              (o,z,z),(o,z,o),(o,o,z)
                                             )
      }
      val pPerEl = nPoints.size

      for (elementNumber <- 1 to elementCount) {
        polygonBlock.append(openPolygon)
        nPoints.foreach(p => {
          region.bind(meshVariable, elementNumber, p._1, p._2, p._3)
          polygonBlock.append(
            region.evaluate(meshEvaluator) match {
              case value : Some[ContinuousValue] =>
                " " + idForNode((value.get.value(0), value.get.value(1), value.get.value(2)))
              case _           => " 0"
            })
        })
        polygonBlock.append(closePolygon)
      }

      (elementCount, nextNodeID - 1)
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

        val (shape : String, nodeCount : Int) = getLibraryShapeName(meshVariable)

        val xyzArray = new StringBuilder()
        val polygonBlock = new StringBuilder()

        if (shape != "shape.unit.cube" && discretisation != 1)
          throw new FmlException("export3DFromFieldML: At present, only cubic meshes can " +
                                 "be exported with discretisation (set discretisation to " +
                                 "1 to avoid this error)");

        val (polygonCount, vertexCount) =
          if (discretisation == 1) {
            simplifyOnly3D(region, meshVariable, meshEvaluator, polygonBlock, xyzArray, shape, elementCount)
          } else {
            subdivide3D(region, meshVariable, meshEvaluator, polygonBlock,
                        xyzArray, discretisation, elementCount)
            (discretisation * discretisation * elementCount,
             2 * (discretisation + 1) * (discretisation + 1) * elementCount)
          }

        val xyzArrayCount = vertexCount * 3
        fillInTemplate(outputName, xyzArray, polygonBlock, shape,
                       polygonCount * (if (wantVolumeMesh) 1 else 2), nodeCount,
                       vertexCount, xyzArrayCount, "trilinearLagrange")
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
        val (shape : String, nodeCount : Int) = getLibraryShapeName(mesh1Variable)

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

        fillInTemplate(outputName, xyzArray, polygonBlock, shape, polygonCount,
                       nodeCount, vertexCount, xyzArrayCount,
                       "trilinearLagrange")
    }
}
