package data

import de.fosd.typechef.conditional.Opt

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:14
 * To change this template use File | Settings | File Templates.
 */

class WhileStatement(a: Condition, b: Program) extends Statement {
  var condition = a
  var doBranch = b
  this.setLabel(condition)


  def calculateFlowGraph(){
       //                         generateBlocks(stm)
                               addInitNode(condition)     //init der condition
                               condition.setLabel(condition)
                               condition.addInitNode(condition)
                               condition.addExitNode(condition)
                               doBranch.addInitNode(condition)     //init des do-block
                           //    addEdge(stm.getLabel, b.b.last.entry.getPositionTo.getLine)     //Kante zum ersten Eintrag nach der SChleife
//TODO condition zu first after doBranch                               addFlow(condition ,b.stmList.last.entry.getPositionTo.getLine)
                           //    addEdge(stm.getLabel, b.b.head.entry.getPositionFrom.getLine)   //Kante zum nächsten Eintrag
                               addSubFlow(calculateFlowWithOps(condition,doBranch.stmList))
                               addExitNode(condition) //final(while) = label(condition)
                           //    addEdge(b.b.last.entry.getPositionFrom.getLine, stm.getLabel)               //Kante zur Condition
                               condition.calculateFlowGraph()
                               doBranch.calculateFlowGraph()   //Program, daher unwichtig welches Exit es hat (wird neu berechnet)
                               addSubFlow(calculateFlowWithOpsReverse(doBranch.stmList, condition))
                               addSubFlow(doBranch.getFlow)
}


  def generateBlocks {
    condition.generateBlocks
    addBlocksSet(condition.getBlocks)
    doBranch.generateBlocks
    addBlocksSet(doBranch.getBlocks)
  }

  override def killAE(caller:AbstractSyntaxTree){
    doBranch.killAE(caller)
  }

  override def genAE{
    condition.genAE
    doBranch.genAE
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

  override def toString:String = "While["+condition.toString+" do"+doBranch.toString+"]"

  override def printKillGen:String = condition.printKillGen+doBranch.printKillGen

  override def printAE:String = condition.printAE+doBranch.printAE

  override def generateAllExpressions{
    condition.generateAllExpressions
    doBranch.generateAllExpressions
    allExpressions++=condition.allExpressions
    allExpressions++=doBranch.allExpressions
  }

  override def setAllExpressions(set:Set[Expression]){
    condition.setAllExpressions(set)
    doBranch.setAllExpressions(set)
    aeEntry++=set
    aeExit++=set
  }

  override def setFeatures{
    doBranch.setFeatures
  }
}

