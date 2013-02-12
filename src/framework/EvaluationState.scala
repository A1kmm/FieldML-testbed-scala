package framework

import scala.collection.mutable.Stack

import fieldml.FieldmlObject
import fieldml.evaluator.Evaluator
import fieldml.evaluator.ArgumentEvaluator

import framework.value.Value
import framework.valuesource._

class EvaluationState[UserDofs]
{
    private val stack = Stack[Context[UserDofs]]()


    private def printContext(depth : Int, c : Context[UserDofs])
    {
        for( i <- 0 until depth ) print( "  " )
        println( c.location )
    }

    
    def printStack
    {
        var depth = 0;
        for( s <- stack.toSeq.reverse )
        {
            printContext( depth, s )
            depth = depth + 1
        }
    }
    
    def pop()
    {
        stack.pop()
    }
    
    
    def pushAndApply( location : String, binds : Seq[(ValueSource[UserDofs], ValueSource[UserDofs])] )
    {
        if (stack.size > 0)
        {
            stack.push(new Context[UserDofs](location, Some(stack.top), binds))
        }
        else
        {
            stack.push(new Context[UserDofs](location, None, binds))
        }
    }
    
    
    def getBind(variable : ValueSource[UserDofs]) : Option[ValueSource[UserDofs]] =
    {
        return stack.top.getBind(variable)
    }
    
    
    private def restart(argument : ValueSource[UserDofs],
                        binds : Seq[(ValueSource[UserDofs], ValueSource[UserDofs])]) : EvaluationState[UserDofs] =
    {
        val newState = new EvaluationState[UserDofs]()
        
        newState.stack.push( new Context( "TEMP for " + argument.name, stack.top.getBindContext( argument ), binds ) )
        
        return newState
    }
    
    
    def resolve(argument : ArgumentEvaluatorValueSource[UserDofs],
                binds : Seq[(ValueSource[UserDofs], ValueSource[UserDofs])]) : Option[UserDofs => Value] =
      getBind(argument).flatMap(_ => {
        val tempState = restart( argument, binds )
        tempState.
          getBind(argument).flatMap(_.asInstanceOf[ValueSource[UserDofs]].evaluate(tempState))
      })
}
