package data

import de.fosd.typechef.conditional.{One, Conditional, Opt}
import de.fosd.typechef.featureexpr.{If, FeatureExpr}
import java.util.ArrayList

/**
 * Created by IntelliJ IDEA.
 * User:
 * Date: 03.10.11
 * Time: 17:12
 * To change this template use File | Settings | File Templates.
 */

class Program(b: List[Opt[AbstractSyntaxTree]]) extends AbstractSyntaxTree {
  var stmList:List[Opt[AbstractSyntaxTree]] = b
  this.setLabel(stmList.head.entry.getLabel)


  def calculateFlowGraph(){
    setInitNodes(calculateInitNodes)
    setExitNodes(calculateExitNodes)
    var actualFeature:FeatureExpr = null
    var remaining:List[Opt[AbstractSyntaxTree]]  = null
    var tmp:AbstractSyntaxTree = null
    for(stm:Opt[AbstractSyntaxTree] <- stmList){
      remaining = stmList.dropWhile(!_.equals(stm)).tail
      tmp = stm.entry
      tmp match{
        case tmp : Program =>
          tmp.calculateFlowGraph()
          addSubFlow(tmp.getFlow)
          for(exit:AbstractSyntaxTree <- tmp.getExitNodes){
            findEdges(exit, remaining)
          }
        case tmp : Ifelse =>
          tmp.calculateFlowGraph()
          addSubFlow(tmp.getFlow)
          for(exit:AbstractSyntaxTree <- tmp.getExitNodes){
            findEdges(exit, remaining)
          }
        case tmp : WhileStatement =>
          tmp.calculateFlowGraph()
          addSubFlow(tmp.getFlow)
          for(exit:AbstractSyntaxTree <- tmp.getExitNodes){
            findEdges(exit, remaining)
          }
        case tmp : Assignment =>
          findEdges(stm.entry, remaining)
      }



    }
  }

  def findEdges(stm:AbstractSyntaxTree, liste:List[Opt[AbstractSyntaxTree]]){
    if(liste==null)
      return
    var edgesByFeature:Set[FeatureExpr] = Set.empty
      for(stmSearch:Opt[AbstractSyntaxTree] <- liste){
          if(stmSearch.feature.equivalentTo(stm.feature)){    //Äquivalentes Feature
            addFlow(stm,stmSearch.entry)
            return
          }else{
            if(stmSearch.feature.and(stm.feature).isSatisfiable() && !edgesByFeature.contains(stmSearch.feature)){   //nicht Äquivalent, aber erfüllbar
               addFlow(stm,stmSearch.entry)
               edgesByFeature += stmSearch.feature
            }
          }
      }
  }


  def nextStm(from:AbstractSyntaxTree):AbstractSyntaxTree = {
    var iterator = stmList.iterator
    while(iterator.hasNext){
      var node = iterator.next
      if(node.entry.equals(from) && iterator.hasNext){
        return iterator.next.entry
      }
    }
    return null
  }

  def flowPossible(targetNode:AbstractSyntaxTree):Boolean = {
    for((from,to)<-this.flow){
      if(targetNode.equals(to)){
        for((fromTmp,toTmp)<-this.flow){
          if(toTmp.equals(targetNode) && !from.equals(fromTmp) && from.getLabel.feature.and(fromTmp.getLabel.feature).isContradiction()){    //Falls es Kanten von !X und X Knoten gibt, dann darf es keine weitere Kante mehr von einem gleichen Knoten (zb true) geben
            return false
          }
        }
      }
    }
    return true
  }

  def getSatisfiableNodeByFeature(node:AbstractSyntaxTree):AbstractSyntaxTree = {
    var lastFoundNode:AbstractSyntaxTree = null
    for(nodeTmp <- stmList.reverse){
      if(nodeTmp.entry.equals(node)){
        return lastFoundNode
      }
      if(nodeTmp.feature.and(node.getLabel.feature).isSatisfiable)
       lastFoundNode = nodeTmp.entry
    }
    return null
  }

  /**
   * Gibt die nächste Node mit dem übergebenen Feature zurück, oder null falls keine existiert.
   * Bedingung: stmList ist sortiert nach Abfolge der Statements
   */
  def getNodeByFeature(node:AbstractSyntaxTree):AbstractSyntaxTree = {
    var lastFoundNode:AbstractSyntaxTree = null
    for(nodeTmp <- stmList.reverse){
      if(nodeTmp.entry.getLabel.equals(node.getLabel)){
        return lastFoundNode
      }
      if(nodeTmp.feature.equivalentTo(node.getLabel.feature))
       lastFoundNode = nodeTmp.entry.getLabel
    }
    return null
  }

  /**
   * Gibt die nächste Node zurück, die dem Feature der übergebenen Node widerspricht (else), oder null falls keine existiert.
   * Bedingung: stmList ist sortiert nach Abfolge der Statements
   */
  def getNodeByContraFeature(node:AbstractSyntaxTree):AbstractSyntaxTree = {
    var lastFoundNode:AbstractSyntaxTree = null
    for(nodeTmp <- stmList.reverse){
      if(nodeTmp.entry.equals(node)){
        return lastFoundNode
      }
      if(nodeTmp.feature.and(node.getLabel.feature).isContradiction())
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

  override def toStringWithoutFeatures:String = {
    var result = ""
    for(stm<-blocks)
      result+=stm.toStringWithoutFeatures
    return result
  }

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
   * Es werden alle feature Informationen aus den Opt Knoten in die AST Knoten kopiert.
   * Beim initialen Aufruf der Methode darf feature null sein, da es nicht benutzt wird.
   */
  override def setFeatures(feature:FeatureExpr) {
    this.feature = feature
    for(stm<-stmList){
      stm.entry.setFeatures(stm.feature)
    }
  }

  override def setFeaturesTrue {
    this.feature = de.fosd.typechef.featureexpr.True
    for(stm <- stmList)
      stm.entry.setFeaturesTrue
  }

  override def filterAeEntry(toFilter:List[AbstractSyntaxTree]) {
      for(stm<-stmList)
        stm.entry.filterAeEntry(toFilter)
    }

  override def filterAeExit(toFilter:List[AbstractSyntaxTree]) {
      for(stm<-stmList)
        stm.entry.filterAeExit(toFilter)
    }

  override def filterBlocks(toFilter:List[Opt[AbstractSyntaxTree]]) {
      var newBlocks:Set[AbstractSyntaxTree] = Set.empty
      for(stm<-stmList){
        stm.entry.filterBlocks(toFilter)
        newBlocks++=stm.entry.getBlocks
      }
      blocks = newBlocks
    }

  def generateFilterFlow(toFilter:List[Opt[AbstractSyntaxTree]]):List[AbstractSyntaxTree] = {
      var filter:List[AbstractSyntaxTree] = List.empty
      for(stm<-toFilter){
        var temp = stm.entry
        temp match{
          case temp:Ifelse =>
            filter = filter.::(temp.condition)
            filter = filter.:::(temp.thenBranch.generateFilterFlow(temp.thenBranch.stmList))
            filter = filter.:::(temp.elseBranch.generateFilterFlow(temp.elseBranch.stmList))
          case temp:WhileStatement =>
            filter = filter.::(temp.condition)
            filter = filter.:::(temp.doBranch.generateFilterFlow(temp.doBranch.stmList))
          case temp:Program => {}
          case _ =>
            filter = filter.::(temp)
        }
      }
    return filter
  }


    def filterFlow(toFilter:List[Opt[AbstractSyntaxTree]]) {
      var newFlow:Set[(AbstractSyntaxTree,AbstractSyntaxTree)] = Set.empty
      var filter:List[AbstractSyntaxTree] = generateFilterFlow(toFilter)
      for((from,to)<-flow){
        if(filter.contains(from.getLabel) && filter.contains(to.getLabel))
          newFlow+=((from,to))
      }
      flow=newFlow
    }



  override def filterGen(toFilter:List[AbstractSyntaxTree]) {
      for(stm<-blocks)
        stm.filterGen(toFilter)
    }

  override def filterKill(toFilter:List[AbstractSyntaxTree]) {
      for(stm<-blocks)
        stm.filterKill(toFilter)
    }


  def compareFlow(flowOne:Set[(AbstractSyntaxTree,AbstractSyntaxTree)], flowTwo:Set[(AbstractSyntaxTree, AbstractSyntaxTree)]) : Boolean = {
    for(tupelOne <- flowOne){
      if(!flowTwo.toString.contains(tupelOne.toString))
        return false
    }
    return true
  }
}