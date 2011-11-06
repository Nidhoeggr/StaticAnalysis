package data

import de.fosd.typechef.conditional.{Conditional, Opt}

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:00
 * To change this template use File | Settings | File Templates.
 */

abstract class Condition() extends Statement {
  var variable:IdentExpression = null
  var expression:Expression = null
  setLabel(this)

   def calculateFlowGraph(){
                               setInitNode(this)
                               expression.setInitNode(getLabel)
//                               if(label!=exitLabel)
//                                  addFlow(getLabel, getPositionTo.getLine)
                               addExitNode(this)  }

  def generateBlocks {
    addBlocks(this)
  }

  override def genAE{
    gen+=expression
  }

  override def printKillGen:String =  getLabel+"Kill: "+kill.toString+"\n"+"Gen: "+gen.toString

  override def printAE:String = getLabel+" AEentry: "+aeEntry.toString+" AEexit: "+aeExit.toString

    override def generateAllExpressions{
        allExpressions+=expression
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
}