package data

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:07
 * To change this template use File | Settings | File Templates.
 */

abstract class Expression extends Statement{

var variable:IdentExpression = null
var expression:Expression = null
var expressions:Set[Expression]=Set.empty
setLabel(this)

def getExpressions:Set[Expression] = expressions
def addExpressions(exp:Set[Expression]) {
  expressions++=exp
}

  override def calculateFlowGraph(){
                               addInitNode(this)
                               expression.addInitNode(this)
//                               if(label!=exitLabel)
//                                  addFlow(getLabel, getPositionTo.getLine)
                               addExitNode(this)  }

  override def generateBlocks {
  }

  override def generateAllExpressions {
    expression.generateAllExpressions
    var tmp = expression.getExpressions
    tmp+=this
    addExpressions(tmp)
  }

  def containsVariable(variable:Expression):Boolean = {
      if(variable.variable.name.eq(this.variable.name)){
        return true
      }else{
        return expression.containsVariable(variable)
      }
    return false
  }

  def containsVariable(variable:IdentExpression):Boolean = {
    var isDefSplit = "def"
    var isThisDefSplit = "def"
    if(variable.name.toString.contains("!")){
      isDefSplit = "!def"
    }
    if(this.variable.name.toString.contains("!")){
      isThisDefSplit = "!def"
    }
    val isDef = variable.name.toString.split(isDefSplit)
    val thisIsDef = this.variable.name.toString.split(isThisDefSplit)
    if(isDef.head.equals(thisIsDef.head)){
        return true
    }
    if(this.variable.name.toString.equals(variable.name.toString)){
        return true
      }else{
        return expression.containsVariable(variable)
      }
  }

    override def printKillGen:String = getLabel+""

    override def setFeatures(feature:FeatureExpr){
      if(variable != null)
        variable.setFeatures(feature)
      if(expression != null)
        expression.setFeatures(feature)
      this.label.feature = feature
    }

}