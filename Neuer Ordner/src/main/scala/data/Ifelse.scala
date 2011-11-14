package data

import de.fosd.typechef.featureexpr.FeatureExpr

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

  def calculateFlowGraph() {
                            //  generateBlocks(stm)
                              //condition.setInitNode(condition)
                              //condition.setLabel(condition)
                              thenBranch.setExitNodes(thenBranch.calculateExitNodes)
                              elseBranch.setExitNodes(elseBranch.calculateExitNodes)
                              condition.setExitNodes(thenBranch.getExitNodes ++ elseBranch.getExitNodes)
//                              addEdge(stm.getLabel, t.b.head.entry.getPositionFrom.getLine)
                              addSubFlow(calculateFlowWithOps(condition, thenBranch.stmList))
                              //condition.addFlow(condition, thenBranch.stmList.head.entry)
//                              addEdge(stm.getLabel, elseBranch.stmList.head.entry.getPositionFrom.getLine)    //Kante von der Condition zum ersten Eintrag des Elsebranchs
                              addSubFlow(calculateFlowWithOps(condition, elseBranch.stmList))
                              //condition.addFlow(condition, elseBranch.stmList.head.entry)
//                              addEdge(t.b.last.entry.getPositionFrom.getLine, e.b.last.entry.getPositionTo.getLine)     //Kante vom letzten Eintrag des Ifbranchs zum ersten Eintrag nach If Else
                              addInitNode(condition)
                              thenBranch.addInitNode(condition)
                              elseBranch.addInitNode(condition)
                              setExitNodes(condition.getExitNodes)
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

  override def toString:String = "If["+condition.toString+" then:"+thenBranch.toString+" else:"+elseBranch.toString+"]"

  override def printKillGen:String = condition.printKillGen+thenBranch.printKillGen+elseBranch.printKillGen

  override def printAE:String = condition.printAE+thenBranch.printAE+elseBranch.printAE

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

    override def setFeatures(feature:FeatureExpr){
      condition.setFeatures(feature)
      thenBranch.setFeatures(feature)
      elseBranch.setFeatures(feature)
      this.label.feature = feature
    }
}