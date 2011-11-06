package data

import de.fosd.typechef.parser.java15.TokenWrapper
import de.fosd.typechef.conditional.Conditional

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:07
 * To change this template use File | Settings | File Templates.
 */

class IntExpression(a: String) extends Expression {
  var name = a
  setLabel(this)

  override def calculateFlowGraph(){
                               addInitNode(getLabel)
                               addExitNode(getLabel)
  }

  override def generateBlocks {
  }

  override def generateAllExpressions {
  }

  override def containsVariable(variable:Expression):Boolean = false
  override def containsVariable(variable:IdentExpression):Boolean = false

  override def toString:String = name

  override def printKillGen:String = getLabel+""


}