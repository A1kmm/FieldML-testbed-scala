package util.library

class ShapeUnitSimplex( val dimensions : Int )
{
    def evaluate(xi : Array[Double]) : Boolean = xi.sum <= 1 && xi.forall(v => v>=0 && v<=1)
}
