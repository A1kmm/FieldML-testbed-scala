package fieldml.valueType.bounds

class ContiguousEnsembleBounds( val count : Int )
    extends EnsembleBounds
{
    override def elementCount = count
}