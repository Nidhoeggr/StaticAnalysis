package data

import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.parser.java15.TokenWrapper

/**
 * Created by IntelliJ IDEA.
 * User:
 * Date: 03.10.11
 * Time: 17:07
 * To change this template use File | Settings | File Templates.
 */

class IdentExpression(a: String) extends Expression {
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

  override def containsVariable(variable:IdentExpression):Boolean = {
    var isDefSplit = "def"
    var isThisDefSplit = "def"
    if(variable.name.toString.contains("!")){
      isDefSplit = "!def"
    }
    if(this.name.toString.contains("!")){
      isThisDefSplit = "!def"
    }
    val isDef = variable.name.toString.split(isDefSplit)
    val thisIsDef = this.name.toString.split(isThisDefSplit)
    if(isDef.head.equals(thisIsDef.head)){
        return true
    }
    if(this.name.toString.equals(variable.name.toString)){
      return true
    }else{
      return false
    }

}

  override def toString:String = name.toString

  override def printKillGen:String = getLabel+""

/*
  override def equals(x:Any):Boolean = {
    x match{
      case x:IdentExpression =>
        return this.name.equals(x.name)

      case _ =>
        return false
    }
  }
*/
  def equalsString(obj: IdentExpression):Boolean = {
    if(name.equals(obj.toString)){
      return true
    }
    return false
  }

}