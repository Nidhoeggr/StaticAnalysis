package data

import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.conditional.Opt

/**
 * Created by IntelliJ IDEA.
 * User:
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
                              thenBranch.setExitNodes(thenBranch.calculateExitNodes)
                              elseBranch.setExitNodes(elseBranch.calculateExitNodes)
                              condition.setExitNodes(thenBranch.getExitNodes ++ elseBranch.getExitNodes)
                              addSubFlow(calculateFlowWithOps(condition, thenBranch.stmList))
                              addSubFlow(calculateFlowWithOps(condition, elseBranch.stmList))
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
      this.feature = feature
      elseBranch.setFeatures(feature)
      this.label.feature = feature
    }

    override def filterAeEntry(toFilter:List[AbstractSyntaxTree]) {
      condition.filterAeEntry(toFilter)
      thenBranch.filterAeEntry(toFilter)
      elseBranch.filterAeEntry(toFilter)
    }

    override def filterAeExit(toFilter:List[AbstractSyntaxTree]) {
      condition.filterAeExit(toFilter)
      thenBranch.filterAeExit(toFilter)
      elseBranch.filterAeExit(toFilter)
    }

    override def filterBlocks(toFilter:List[Opt[AbstractSyntaxTree]]) {
      condition.filterBlocks(toFilter)
      thenBranch.filterBlocks(toFilter)
      elseBranch.filterBlocks(toFilter)
    }

    override def filterGen(toFilter:List[AbstractSyntaxTree]) {
      condition.filterGen(toFilter)
      thenBranch.filterGen(toFilter)
      elseBranch.filterGen(toFilter)
    }

    override def filterKill(toFilter:List[AbstractSyntaxTree]) {
      condition.filterKill(toFilter)
      thenBranch.filterKill(toFilter)
      elseBranch.filterKill(toFilter)
    }

  override def setFeaturesTrue {
    label.setFeaturesTrue
    condition.setFeaturesTrue
    this.feature = de.fosd.typechef.featureexpr.True
    thenBranch.setFeaturesTrue
    elseBranch.setFeaturesTrue
  }

}