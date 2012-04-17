package data

import de.fosd.typechef.parser.java15.TokenWrapper
import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * Created by IntelliJ IDEA.
 * User:
 * Date: 03.10.11
 * Time: 17:07
 * To change this template use File | Settings | File Templates.
 */

class IntExpression(a: String) extends Expression {
  var name = a
  setLabel(this)

  override def calculateFlowGraph(){
                               addInitNode(this)
                               addExitNode(this)
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