package data

import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.parser.java15.TokenWrapper

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:10
 * To change this template use File | Settings | File Templates.
 */

class Assignment(n: Conditional[TokenWrapper], exp: Conditional[TokenWrapper]) extends Statement {
  var name = n
  var expression = exp

  this.setLabel(this)



  def calculateFlowGraph() {
              //                  generateBlocks(stm)
                               setInitNode(getLabel)
                               expression.setInitNode(getLabel)
//                               if(label!=exitLabel)
//                                  addFlow(getLabel, getPositionTo.getLine)
                               addExitNode(getLabel)
}

  def generateBlocks {
    addBlocks(this)
  }

override def killAE(caller:AbstractSyntaxTree) {
  for(exp<-caller.allExpressions){
    if(exp.containsVariable(name)){
         kill+=exp
    }
  }
}

  override def genAE{
    if(!expression.containsVariable(name)){
      gen+=expression
    }
  }

  override def calculateAEentry(prog:Program):Set[AbstractSyntaxTree] ={
    var aeExitIntersection:Set[AbstractSyntaxTree]=null
    for((from,to)<-prog.getFlow){
      if(to.equals(this)){
        if(aeExitIntersection == null){
          aeExitIntersection=from.calculateAEexit(prog)
        }else{
          aeExitIntersection=from.calculateAEexit(prog) & aeExitIntersection
        }
      }
    }
    if(aeExitIntersection != null){
      aeEntry = aeExitIntersection
    }
    return aeExitIntersection
  }

  override def calculateAEexit(prog:Program):Set[AbstractSyntaxTree] = {
    var aeEntryKill:Set[AbstractSyntaxTree] = aeEntry--kill
    var aeEntryUnionGen:Set[AbstractSyntaxTree] = aeEntryKill ++ gen
    aeExit = aeEntryUnionGen
    return aeEntryUnionGen
  }

  override def toString:String = "Ass["+name.toString+exp.toString+"]"

  override def printKillGen:String = getLabel+"Kill: "+kill.toString+"\n"+"Gen: "+gen.toString

  override def printAE:String = getLabel+" AEentry: "+aeEntry.toString+" AEexit: "+aeExit.toString

    override def generateAllExpressions{
      allExpressions+=expression
  }

}