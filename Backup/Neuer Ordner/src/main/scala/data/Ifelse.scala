package data

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:10
 * To change this template use File | Settings | File Templates.
 */

class Ifelse(cond: Condition, thenB: Program, elseB: Program) extends Statement {
  var condition:Condition = cond
  var thenBranch:Program = thenB
  var elseBranch:Program = elseB
  this.setLabel(condition)

  //TODO Trennung zwischen then und else Block
  def calculateFlowGraph() {
                            //  generateBlocks(stm)
                              //condition.setInitNode(condition)
                              //condition.setLabel(condition)
                              condition.addExitNode(thenBranch.stmList.last.entry.getLabel)
                              condition.addExitNode(elseBranch.stmList.last.entry.getLabel)
//                              addEdge(stm.getLabel, t.b.head.entry.getPositionFrom.getLine)
                              addFlow(condition, thenBranch.stmList.head.entry.getLabel)
                              //condition.addFlow(condition, thenBranch.stmList.head.entry)
//                              addEdge(stm.getLabel, elseBranch.stmList.head.entry.getPositionFrom.getLine)    //Kante von der Condition zum ersten Eintrag des Elsebranchs
                              addFlow(condition, elseBranch.stmList.head.entry.getLabel)
                              //condition.addFlow(condition, elseBranch.stmList.head.entry)
//                              addEdge(t.b.last.entry.getPositionFrom.getLine, e.b.last.entry.getPositionTo.getLine)     //Kante vom letzten Eintrag des Ifbranchs zum ersten Eintrag nach If Else
                              setInitNode(condition)
                              thenBranch.setInitNode(condition)
                              elseBranch.setInitNode(condition)
                              addExitNode(thenBranch.stmList.last.entry.getLabel)    //final(ifelse) =
                              addExitNode(elseBranch.stmList.last.entry.getLabel)   // final(if) U final(else)
                              thenBranch.calculateFlowGraph();   //Kanten des Ifbranchs berechnen
                              elseBranch.calculateFlowGraph();   //Kanten des ElseBranchs berechnen
                              condition.calculateFlowGraph()
                              addSubFlow(thenBranch.getFlow)
                              addSubFlow(elseBranch.getFlow)
  }

   def generateBlocks {
    condition.generateBlocks
    addBlocksSet(condition.getBlocks)
    thenBranch.generateBlocks
    addBlocksSet(thenBranch.getBlocks)
    elseBranch.generateBlocks
    addBlocksSet(elseBranch.getBlocks)
  }

  override def killAE(caller:AbstractSyntaxTree){
    thenBranch.killAE(caller)
    elseBranch.killAE(caller)
  }

  override def genAE{
    condition.genAE
    thenBranch.genAE
    elseBranch.genAE
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


  override def toString:String = "If["+condition.toString+" then:"+thenBranch.toString+" else:"+elseBranch.toString+"]"

  override def printKillGen:String = condition.printKillGen+"\n"+thenBranch.printKillGen+"\n"+elseBranch.printKillGen

  override def printAE:String = condition.printAE+"\n"+thenBranch.printAE+"\n"+elseBranch.printAE

  override def generateAllExpressions {
    condition.generateAllExpressions
    thenBranch.generateAllExpressions
    elseBranch.generateAllExpressions
    allExpressions++=condition.allExpressions
    allExpressions++=thenBranch.allExpressions
    allExpressions++=elseBranch.allExpressions
  }

  override def setAllExpressions(set:Set[Expression]){
    condition.setAllExpressions(set)
    thenBranch.setAllExpressions(set)
    elseBranch.setAllExpressions(set)
    aeEntry++=set
    aeExit++=set
  }
}