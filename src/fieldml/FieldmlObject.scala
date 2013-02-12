package fieldml

import scala.collection.mutable.Map

class FieldmlObject(nname : => String)
{
    lazy val name : String = nname

    var isLocal : Boolean = true
    
    val markup : Map[String, String] = Map[String, String]()
}
