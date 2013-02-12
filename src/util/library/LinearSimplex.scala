package util.library

class LinearSimplex()
{
    private def basisFunction( xi : Array[Double] ) : Array[Double] =
      Array(1 - xi.sum) ++ xi

    private def dotProduct( a1 : Array[Double], a2 : Array[Double] ) : Double =
      a1.zip(a2).map(((_:Double)*(_:Double)).tupled).sum
    
    def evaluate( xi : Array[Double], params : Array[Double] ) : Array[Double] =
      Array(dotProduct(basisFunction(xi), params))
}
