package util.library

class QuadraticSimplex(val dimensions : Int)
{
    private def basisFunction( xi : Array[Double] ) : Array[Double] =
    {
        val xm1 = 1 - xi.sum
        def x(i : Int) : Double = if (i >= dimensions) 0 else xi(i)
        Array((2 * xm1 - 1) * xm1, 4 * xm1 * x(0), (2*x(0)-1)*x(0),
              4 * xm1 * x(1), 4 * x(0) * x(1), (2 * x(1) - 1) * x(1),
              4 * xm1 * x(2), 4 * x(0) * x(2), 4 * x(1) * x(2),
              (2 * x(2) - 1) * x(2)
             )
    }
    
    private def dotProduct( a1 : Array[Double], a2 : Array[Double] ) : Double =
      a1.zip(a2).map(((_:Double)*(_:Double)).tupled).sum
    
    def evaluate( xi : Array[Double], params : Array[Double] ) : Array[Double] =
      Array(dotProduct(basisFunction(xi), params))
}
