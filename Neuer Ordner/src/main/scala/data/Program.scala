package data

import de.fosd.typechef.conditional.{One, Conditional, Opt}
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:12
 * To change this template use File | Settings | File Templates.
 */

class Program(b: List[Opt[Statement]]) extends AbstractSyntaxTree {
  var stmList:List[Opt[AbstractSyntaxTree]] = b
  this.setLabel(stmList.head.entry.getLabel)


  def calculateFlowGraph(){
    var oldStm:AbstractSyntaxTree = null
    var oldDefStmElse:Opt[AbstractSyntaxTree] = null    //zwei old Opt Knoten für then/else branch
    var oldDefStmThen:Opt[AbstractSyntaxTree] = null
    setInitNodes(calculateInitNodes)
    setExitNodes(calculateExitNodes)
    for(stm:Opt[AbstractSyntaxTree]<-stmList){
      stm match{
        case  de.fosd.typechef.conditional.Opt(feature,entry) =>
          feature match{
            case de.fosd.typechef.featureexpr.True => {
              stm.entry.calculateFlowGraph()
               if(oldStm!=null){//} && oldDefStmThen == null && oldDefStm == null){
                 if(oldStm.getLabel.getExitNodes.isEmpty)
                    addFlow(oldStm,stm.entry)
                 for(node<-oldStm.getLabel.getExitNodes)
                    addFlow(node,stm.entry)
               }
               oldStm = stm.entry
               addSubFlow(stm.entry.getFlow)
               if(oldDefStmThen!=null){
                 addFlow(oldDefStmThen.entry,stm.entry)                    //Kante vom letzten Knoten des Opt-thenBranchs zum nächsten normalen Knoten
                 oldDefStmThen = null
               }
               if(oldDefStmElse!=null){
                 addFlow(oldDefStmElse.entry, stm.entry)
                 oldDefStmElse = null
               }
              /*
               if(oldDefStm!=null){
                 addFlow(oldDefStm.entry, stm.entry.getLabel)
                 oldDefStm = null
               }
               */
            }
            case _ =>  {     //Optinaler Knoten
              var x = feature
              stm.entry.calculateFlowGraph()
              val nextNodeWithSameFeature = getNodeByFeature(stm.entry)
              if(nextNodeWithSameFeature!=null){
                addFlow(stm.entry, nextNodeWithSameFeature)
              }
              if((oldDefStmThen != null && oldDefStmThen.feature.and(stm.feature).isSatisfiable())){      //Wenn es einen alten Opt Knoten gab und er das selbe feature hat, dann packe ihn auf den flußgraph
                addFlow(oldDefStmThen.entry,stm.entry)
              }else{                                                        //Falls nicht, kann es eine Kante vom letzten nicht Opt Knoten zum Opt Knoten geben
                if(oldStm != null)
                  addFlow(oldStm, stm.entry)
                if(oldDefStmElse != null && oldDefStmElse.feature.and(stm.feature).isSatisfiable())           //ElseBranch
                  addFlow(oldDefStmElse.entry, stm.entry)
              }
              if(oldDefStmThen != null && !oldDefStmThen.feature.and(stm.feature).isSatisfiable()){
                oldDefStmElse = stm
              }else{
                oldDefStmThen = stm
              }
              addSubFlow(stm.entry.getFlow)
            }
          }
      }
    }
  }

  /**
   * Gibt die nächste Node mit dem übergebenen Feature zurück, oder null falls keine existiert.
   * Bedingung: stmList ist sortiert nach Abfolge der Statements
   */
  def getNodeByFeature(node:AbstractSyntaxTree):AbstractSyntaxTree = {
    var lastFoundNode:AbstractSyntaxTree = null
    for(nodeTmp <- stmList.reverse){
      if(nodeTmp.entry.equals(node)){                           //Optimierung, um nicht alles zu durchlaufen
        return lastFoundNode
      }
      if(nodeTmp.feature.equivalentTo(node.getLabel.feature))    //Vergleich auf Position, um Gleichheit und vorherige Nodes auszuschließen
       lastFoundNode = nodeTmp.entry
    }
    return null
  }


  def calculateExitNodes:Set[AbstractSyntaxTree] = {
    var possibleExits:Set[AbstractSyntaxTree] = exitNodes
    for(stm <- stmList.reverse){
      if(stm.entry.getLabel.feature.equivalentTo(this.getLabel.feature)) {
          possibleExits+=stm.entry
          return possibleExits
      } else{
          possibleExits+=stm.entry
      }
  }
    return possibleExits
  }

  def calculateInitNodes:Set[AbstractSyntaxTree] = {
    var possibleInits:Set[AbstractSyntaxTree] = initNodes
    for(stm <- stmList){
      if(stm.entry.getLabel.feature.equivalentTo(this.getLabel.feature)) {
          possibleInits+=stm.entry
          return possibleInits
      } else{
          possibleInits+=stm.entry
      }
  }
    return possibleInits
  }

  def generateBlocks {
    for(stm:Opt[AbstractSyntaxTree]<-stmList){
      stm.entry.generateBlocks
      addBlocksSet(stm.entry.getBlocks)
    }
  }

  override def generateAllExpressions{
    for(stm:Opt[AbstractSyntaxTree]<-stmList){
      stm.entry.generateAllExpressions
      allExpressions++=stm.entry.allExpressions
    }
  }


  override def genAE{
    for(stm:Opt[AbstractSyntaxTree]<-stmList){
      stm.entry.genAE
    }
  }

    override def killAE(caller:AbstractSyntaxTree){
    for(stm:Opt[AbstractSyntaxTree]<-stmList){
      stm.entry.killAE(caller)
    }
  }

  override def calculateAEentry(prog:Program):Set[AbstractSyntaxTree] ={

      for(stm<-blocks){
        stm.calculateAEentry(this)
      }
    return Set.empty
  }

  override def calculateAEexit(prog:Program):Set[AbstractSyntaxTree] = {
      for(stm<-blocks){
        stm.calculateAEexit(prog)
      }
    return Set.empty
  }

  override def toString:String = "\n"+stmList.toString()

  override def printKillGen:String = {
    var result:String = ""
    for(stm<-blocks) {
      result+=stm.printKillGen
    }
    return result
  }

  override def printAE:String = {
    var result:String = ""
    for(stm<-blocks) {
      result+=stm.printAE
    }
    return result
  }

  override def setAllExpressions(set:Set[Expression]){
    aeEntry++=set
    aeExit++=set
    for(stm<-blocks){
      stm.setAllExpressions(set)
    }
  }

  def getStmlist = stmList

  /**
   * Beim initialen Aufruf darf feature null sein.
   */
  override def setFeatures(feature:FeatureExpr) {
    for(stm<-stmList){
      stm.entry.setFeatures(stm.feature)
    }
  }

}