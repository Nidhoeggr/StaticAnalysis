package data

import de.fosd.typechef.parser.java15.TokenWrapper
import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.featureexpr.FeatureExpr

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

  /*
  override def equals(x:Any):Boolean = {
    x match{
      case x:IntExpression =>
        return this.name.equals(x.name)

      case _ =>
        return false
    }
  }
  */
}