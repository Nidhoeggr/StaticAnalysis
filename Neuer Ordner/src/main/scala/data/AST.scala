package data

import de.fosd.typechef.parser._
import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 21.09.11
 * Time: 16:26
 * To change this template use File | Settings | File Templates.
 */
object AbstractSyntaxTree extends WithPosition{
  private var number:Int = 0
  def incNumber:Int = {
    number += 1
    return number
  }

}


abstract class AbstractSyntaxTree extends WithPosition  {

var feature:FeatureExpr = null
var initNodes : Set[AbstractSyntaxTree] = Set.empty
var exitNodes : Set[AbstractSyntaxTree] = Set.empty
//var flow : List[(AbstractSyntaxTree,AbstractSyntaxTree)] = List.empty
var flow : Set[(AbstractSyntaxTree,AbstractSyntaxTree)] = Set.empty
var label : AbstractSyntaxTree = null
var aeEntry : Set[AbstractSyntaxTree] = Set.empty
var aeExit : Set[AbstractSyntaxTree] = Set.empty
var gen : Set[AbstractSyntaxTree] = Set.empty
var kill : Set[AbstractSyntaxTree] = Set.empty
var blocks:Set[AbstractSyntaxTree] =Set.empty
var number:Int = AbstractSyntaxTree.incNumber

var allExpressions:Set[Expression] = Set.empty

def getAllExpressions = allExpressions

def setAllExpressions(set:Set[Expression]){
    aeEntry++=set
    aeExit++=set
}

def getGen = gen
def getKill = kill


def generateAllExpressions{}
def genAE{}
def killAE(caller:AbstractSyntaxTree){}
def calculateFlowGraph()
def generateBlocks
def calculateAEentry(prog:Program):Set[AbstractSyntaxTree] = Set.empty
def calculateAEexit(prog:Program):Set[AbstractSyntaxTree] = Set.empty

def addBlocks(block:AbstractSyntaxTree){ blocks+=block }

def addBlocksSet(block:Set[AbstractSyntaxTree]) {blocks++=block}

def getBlocks : Set[AbstractSyntaxTree] = blocks

def setEntry(entry : Set[AbstractSyntaxTree]) {
        aeEntry++=entry
}

def getEntry : Set[AbstractSyntaxTree] =
        aeEntry

def setExit(exit : Set[AbstractSyntaxTree]) {
        aeExit++=exit
}

def getExit : Set[AbstractSyntaxTree] =
        aeExit

def setInitNodes(nodes:Set[AbstractSyntaxTree]) {
        initNodes = nodes
}

def addInitNode(nodes:AbstractSyntaxTree) {
  initNodes+= nodes
}

def getInitNodes:Set[AbstractSyntaxTree] = initNodes

def setExitNodes(nodes:Set[AbstractSyntaxTree]) {
        exitNodes = nodes
}

def addExitNode(node:AbstractSyntaxTree) {
        exitNodes+=node
}

def getExitNodes:Set[AbstractSyntaxTree] = exitNodes

def setFlow(flow:Set[(AbstractSyntaxTree,AbstractSyntaxTree)]) {
        this.flow = flow
}

def addFlow(from:AbstractSyntaxTree, to:AbstractSyntaxTree) {
  from match{
    case from:Ifelse =>
      for(x<-from.getExitNodes)              //da bei ifelse nicht das label, sondern die exitnodes der Branches verknüpft werden dürfen
        flow+=((x,to))
    case _ =>
       flow+=((from,to))
  }
}

def addSubFlow(subFlow:Set[(AbstractSyntaxTree,AbstractSyntaxTree)]) {
       flow++=subFlow
}

def getFlow:Set[(AbstractSyntaxTree,AbstractSyntaxTree)] = flow

def setLabel(node:AbstractSyntaxTree) {
        label = node
}

def getLabel:AbstractSyntaxTree = label

def printKillGen:String
def printAE:String = ""

    /**
   * Vergleich per ID, die in AbstractSyntaxTree zugewiesen wird
   */
  override def equals(that:Any):Boolean = {
    that match {
      case x:AbstractSyntaxTree =>
        return this.getNumber == x.getNumber
      case _ => {
        return false
      }
    }
  }

//def equalsString(obj:AbstractSyntaxTree):Boolean ={
//  return this.toString.equals(obj.toString)  }

  def printFlow:String = {
    var result:String = ""
    for(x <- this.flow){
      result+=x.toString+"\n"
    }
    return result
  }

  def getNumber:Int = number

  /*
  Berechnet alle Kanten von einer Condition (if oder while) zu den möglichen nächsten Knoten
   */
  def calculateFlowWithOps(from:AbstractSyntaxTree, allStm:List[Opt[AbstractSyntaxTree]]): Set[(AbstractSyntaxTree,AbstractSyntaxTree)] = {
    var result:Set[(AbstractSyntaxTree, AbstractSyntaxTree)] = Set.empty
    var feature:FeatureExpr = null
    for(stm <- allStm){
      if(stm.entry.getLabel.feature.equivalentTo(from.getLabel.feature)){
          result+=((from,stm.entry.getLabel))
          return result
      }else{
          if(feature!=null){                                            //erste Iteration abfangen
            if(!feature.equivalentTo(stm.entry.getLabel.feature))       //Nur eine Kante pro gleichem Statement (nicht weiter in die Sequenz verweisen)
              result+=((from,stm.entry.getLabel))
          }else{
            result+=((from,stm.entry.getLabel))
          }
          feature=stm.entry.getLabel.feature
      }


/*
      stm.feature match {
        case de.fosd.typechef.featureexpr.True =>
          result+=((from,stm.entry.getLabel))
          return result
        case _ =>
          result+=((from,stm.entry.getLabel))
      }
*/
    }
    return result
  }

  /*
  Berechnet alle Kanten von den "letzten" Knoten eines WhileStatements zur Condition
   */
  def calculateFlowWithOpsReverse(allStm:List[Opt[AbstractSyntaxTree]], to:AbstractSyntaxTree): Set[(AbstractSyntaxTree,AbstractSyntaxTree)] = {
    var result:Set[(AbstractSyntaxTree, AbstractSyntaxTree)] = Set.empty
    var feature:FeatureExpr = null
    for(stm <- allStm.reverse){
      if(stm.entry.getLabel.feature.equivalentTo(to.getLabel.feature)){
          result+=((stm.entry.getLabel, to))
          return result
      }else{
        if(feature!=null){                                            //erste Iteration abfangen
          if(!feature.equivalentTo(stm.entry.getLabel.feature))       //Nur eine Kante pro gleichem Statement (nicht weiter in die Sequenz verweisen)
            result+=((stm.entry.getLabel, to))
        }else{
          result+=((stm.entry.getLabel, to))
        }
        feature=stm.entry.getLabel.feature
    }
/*
      match {
        case de.fosd.typechef.featureexpr.True =>
          result+=((stm.entry.getLabel, to))
          return result
        case _ =>
          if(feature)
          result+=((stm.entry.getLabel, to))
      }
*/
    }
    return result
  }

  def setFeatures{}
/*
  def matchAST(x:Conditional[AbstractSyntaxTree]):AbstractSyntaxTree = {
    x match{
      case x:Conditional[Assignment] => return Assignment
      case x:Conditional[WhileStatement] => return WhileStatement
      case _ => return null
    }
  }
*/
}