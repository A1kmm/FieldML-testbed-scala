package util

object SimpleFieldMLExporter extends MeshExporter
{
  override val wantVolumeMesh = true
  override val startIndicesAt = 1
  override val rawXml = """<?xml version="1.0" encoding="UTF-8"?>
<Fieldml version="0.5"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xsi:noNamespaceSchemaLocation="http://www.fieldml.org/resources/xml/0.5/FieldML_0.5.xsd">
<Region name="$name">

<!-- make types and evaluators from the library visible in this region
  under local names -->
<Import xlink:href="http://www.fieldml.org/resources/xml/0.5/FieldML_Library_0.5.xml" region="library">
  <ImportType localName="real.type" remoteName="real.1d" />
  <ImportType localName="interp.parameters" remoteName="parameters.3d.unit.$interpolator" />
  <ImportType localName="interp.points" remoteName="parameters.3d.unit.$interpolator.component" />
  <ImportType localName="coordinates.rc.3d.component" remoteName="coordinates.rc.3d.component" />
  <ImportType localName="coordinates.rc.3d" remoteName="coordinates.rc.3d" />
  <ImportEvaluator localName="interp.parameters.argument" remoteName="parameters.3d.unit.$interpolator.argument" />
  <ImportEvaluator localName="interp.points.argument" remoteName="parameters.3d.unit.$interpolator.component.argument" />
  <ImportEvaluator localName="interp.interpolator" remoteName="interpolator.3d.unit.$interpolator" />
  <ImportEvaluator localName="chart.3d.argument" remoteName="chart.3d.argument" />
  <ImportEvaluator localName="coordinates.rc.3d.component.argument" remoteName="coordinates.rc.3d.component.argument" />
  <ImportEvaluator localName="$shape" remoteName="$shape"/>
</Import>

<!-- define $vertextCount-member ensemble to represent nodes 1..$vertexCount -->
<EnsembleType name="$name.nodes">
  <Members>
    <MemberRange min="1" max="$vertexCount" />
  </Members>
</EnsembleType>

<!-- declare an argument of nodes type; this represents a value source
  that other evaluators can map their values to -->
<ArgumentEvaluator name="$name.nodes.argument" valueType="$name.nodes" />

<!-- declare an argument representing the node-indexed DOFs for a field
  template, a real value expected to be a function of "$name.nodes.argument" -->
<ArgumentEvaluator name="$name.node.dofs.argument" valueType="real.type">
  <Arguments>
    <Argument name="$name.nodes.argument" />
  </Arguments>
</ArgumentEvaluator>

<!-- define a 3-D mesh type with $polygonCount elements... -->
<MeshType name="$name.mesh.type">
  <Elements name="elements">
    <Members>
      <MemberRange min="1" max="$polygonCount" />
    </Members>
  </Elements>
  <Chart name="chart">
    <Components name="$name.mesh.type.chart.component" count="3" />
  </Chart>
  <Shapes evaluator="$shape"/>
</MeshType>

<!-- declare an argument of the mesh type. Evaluators varying only with this
  argument are interpreted as fields over the mesh -->
<ArgumentEvaluator name="$name.mesh.argument" valueType="$name.mesh.type" />

<!-- An inline data resource listing the node IDs for each element. Resources
  are just raw data; a ParameterEvaluator is needed to add semantic meaning -->
<DataResource name="$name.nodes.connectivity.resource">
  <DataResourceDescription>
    <DataResourceString>
      $polygonBlock
    </DataResourceString>
  </DataResourceDescription>
  <ArrayDataSource name="$name.nodes.connectivity.data" location="1" rank="2">
    <RawArraySize>
      $polygonCount 8
    </RawArraySize>
  </ArrayDataSource>
</DataResource>

<!-- define mapping from element*localnode to global index from ensemble
  "$name.nodes.argument". "interp.points" are documented
  as being the 8 corner points of a unit cube at chart locations:
  (0,0,0), (1,0,0), (0,1,0), (1,1,0), (0,0,1), (1,0,1), (0,1,1), (1,1,1) -->
<ParameterEvaluator name="$name.interp.connectivity" valueType="$name.nodes">
  <DenseArrayData data="$name.nodes.connectivity.data">
    <DenseIndexes>
      <IndexEvaluator evaluator="$name.mesh.argument.elements" />
      <IndexEvaluator evaluator="interp.points.argument" />
    </DenseIndexes>
  </DenseArrayData>
</ParameterEvaluator>

<!-- construct a vector of node parameters to pass on to
  "$name.trilinear.interpolator" -->
<AggregateEvaluator name="$name.interp.parameters"
    valueType="interp.parameters">
  <Bindings>
    <BindIndex argument="interp.points.argument" indexNumber="1" />
    <Bind argument="$name.nodes.argument" source="$name.interp.connectivity" />
  </Bindings>
  <ComponentEvaluators default="$name.node.dofs.argument" />
</AggregateEvaluator>

<!-- define evaluator returning value of library trilinear Lagrange interpolator
  at the element chart location of mesh type "$name.mesh" and using parameters
  from evaluator "$name.interp.parameters". -->
<ReferenceEvaluator name="$name.trilinear.interpolator"
    evaluator="interp.interpolator" valueType="real.type">
  <Bindings>
    <Bind argument="chart.3d.argument" source="$name.mesh.argument.chart" />
    <Bind argument="interp.parameters.argument" source="$name.interp.parameters" />
  </Bindings>
</ReferenceEvaluator>

<!-- define a piecewise template delegating which evaluator gives the template
  its values in each element, which is trivial for this one element mesh.
  It is a template for a field defined over the mesh represented by
  "$name.mesh.argument", with the unbound parameter source
  "$name.node.dofs.argument" inherited from delegate evaluator
  "$name.trilinear.interpolator" -->
<PiecewiseEvaluator name="$name.template.trilinear" valueType="real.type">
  <IndexEvaluators>
    <IndexEvaluator evaluator="$name.mesh.argument.elements" indexNumber="1" />
  </IndexEvaluators>
  <EvaluatorMap default="$name.trilinear.interpolator" />
</PiecewiseEvaluator>

<!-- inline data resource listing raw values for the $vertexCount nodes * 3 components
  of the 'coordinates' field. ParameterEvaluator "$name.node.coordinates"
  gives the data semantic meaning. -->
<DataResource name="$name.coordinates.resource">
  <DataResourceDescription>
    <DataResourceString>
$xyzArray
    </DataResourceString>
  </DataResourceDescription>
  <ArrayDataSource name="$name.coordinates.data" location="1" rank="2">
    <RawArraySize>
      $vertexCount 3
    </RawArraySize>
  </ArrayDataSource>
</DataResource>

<!-- parameters for the coordinate field, listing a scalar real parameter
  for all permutations of library 3-component ensemble
  "coordinates.rc.3d.component" and $vertexCount-member ensemble "$name.nodes.argument" -->
<ParameterEvaluator name="$name.node.coordinates" valueType="real.type">
  <DenseArrayData data="$name.coordinates.data">
    <DenseIndexes>
      <IndexEvaluator evaluator="$name.nodes.argument" />
      <IndexEvaluator evaluator="coordinates.rc.3d.component.argument" />
    </DenseIndexes>
  </DenseArrayData>
</ParameterEvaluator>

<!-- define the final vector coordinates field by aggregating evaluators for
  each component of the vector valueType. Although each component uses the
  same evaluator in this example, they produce different values because the
  parameters on which they depend vary with the same component ensemble
  argument ("coordinates.rc.3d.component.argument"). -->
<AggregateEvaluator name="coordinates" valueType="coordinates.rc.3d">
  <Bindings>
    <BindIndex argument="coordinates.rc.3d.component.argument" indexNumber="1" />
    <Bind argument="$name.node.dofs.argument" source="$name.node.coordinates" />
  </Bindings>
  <ComponentEvaluators default="$name.template.trilinear" />
</AggregateEvaluator>

</Region>
</Fieldml>"""
    override val openPolygon = ""
    override val closePolygon = "\n"
}
