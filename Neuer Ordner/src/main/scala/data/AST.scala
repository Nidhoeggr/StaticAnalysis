package data

import de.fosd.typechef.parser._
import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.featureexpr.{DefinedExpr, FeatureExpr}
import collection.mutable.Map

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
def toStringWithoutFeatures:String = toString
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

  /**
   * From sollte
   */
def addFlow(from:AbstractSyntaxTree, to:AbstractSyntaxTree) {
  from match{
    case from:Ifelse =>
      for(x<-from.getExitNodes)              //da bei ifelse nicht das label, sondern die exitnodes der Branches verknüpft werden dürfen
        flow+=((x.getLabel,to.getLabel))
    case _ =>
       flow+=((from.getLabel,to.getLabel))
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
   * Vergleich per ID, die in AbstractSyntaxTree zugewiesen wird. Assignments werden auf textuelle Gleichheit und gleiche Features geprüft.
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
          result+=((from,stm.entry))
          return result
      }else{
          if(feature!=null){                                            //erste Iteration abfangen
            if(!feature.equivalentTo(stm.entry.getLabel.feature))       //Nur eine Kante pro gleichem Statement (nicht weiter in die Sequenz verweisen)
              result+=((from,stm.entry))
          }else{
            result+=((from,stm.entry))
          }
          feature=stm.entry.getLabel.feature
      }
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
          result+=((stm.entry, to))
          return result
      }else{
        if(feature!=null){                                            //erste Iteration abfangen
          if(!feature.equivalentTo(stm.entry.getLabel.feature))       //Nur eine Kante pro gleichem Statement (nicht weiter in die Sequenz verweisen)
            result+=((stm.entry, to))
        }else{
          result+=((stm.entry.getLabel, to))
        }
        feature=stm.entry.getLabel.feature
      }
    }
    return result
  }

  def setFeatures(feature:FeatureExpr){
    this.feature = feature
    this.getLabel.feature = feature
  }

  /**
   * VORSICHT: die Methode ist keine deep-copy Methode, sondern eine Hilfsmethode für die Testinfrastruktur. Es wird eine spezielle Kopie für die Analyse erzeugt. Dabei werden
   * einige Felder zurückgesetzt und nicht kopiert.
   * Erzeugt eine Kopie des AbstractSyntaxTrees auf dem die Methode gerufen wird.(Vorsicht auch Number wird kopiert auf der die Methode equals arbeitet). Vorsicht label wird nicht
   * geclont.
   */
  def copy : AbstractSyntaxTree = {
    var cloned:AbstractSyntaxTree = null
    val toClone:AbstractSyntaxTree = this
    toClone match {
      case toClone:AddExpression =>
        cloned = new AddExpression(toClone.variable, toClone.expression)
      case toClone:Assignment =>
        cloned = new Assignment(toClone.name, toClone.expression)
      case toClone:ConditionEquals =>
        cloned = new ConditionEquals(toClone.variable, toClone.expression)
      case toClone:ConditionGreater =>
        cloned = new ConditionGreater(toClone.variable, toClone.expression)
      case toClone:ConditionGreaterOrEquals =>
        cloned = new ConditionGreaterOrEquals(toClone.variable, toClone.expression)
      case toClone:ConditionLesser =>
        cloned = new ConditionLesser(toClone.variable, toClone.expression)
      case toClone:ConditionLessOrEquals =>
        cloned = new ConditionLessOrEquals(toClone.variable, toClone.expression)
      case toClone:DivExpression =>
        cloned = new DivExpression(toClone.variable, toClone.expression)
      case toClone:IdentExpression =>
        cloned = new IdentExpression(toClone.name)
      case toClone:Ifelse =>
        cloned = new Ifelse(toClone.condition, toClone.thenBranch, toClone.elseBranch)
      case toClone:IntExpression =>
        cloned = new IdentExpression(toClone.name)
      case toClone:MulExpression =>
        cloned = new MulExpression(toClone.variable, toClone.expression)
      case toClone:Program =>
        cloned = new Program(toClone.stmList)
      case toClone:SubExpression =>
        cloned = new SubExpression(toClone.variable, toClone.expression)
      case toClone:WhileStatement =>
        cloned = new WhileStatement(toClone.condition, toClone.doBranch)
    }
      cloned.aeEntry = Set.empty
      cloned.aeExit = Set.empty
      cloned.allExpressions = Set.empty
      cloned.blocks = Set.empty
      cloned.exitNodes = Set.empty
      cloned.feature = toClone.feature
      cloned.flow = Set.empty
      cloned.gen = Set.empty
      cloned.initNodes = Set.empty
      cloned.kill = Set.empty
      cloned.number = toClone.number
      cloned.range = toClone.range
    return cloned

  }

  /**
   * VORSICHT: die Methode ist keine deep-copy Methode, sondern eine Hilfsmethode für die Testinfrastruktur. Es wird eine spezielle Kopie für die Analyse erzeugt. Dabei werden
   * einige Felder zurückgesetzt und nicht kopiert. In dieser Version hier werden AE relevante Felder nicht zurück gesetzt.
   * Erzeugt eine Kopie des AbstractSyntaxTrees auf dem die Methode gerufen wird.(Vorsicht auch Number wird kopiert auf der die Methode equals arbeitet). Vorsicht label wird nicht
   * geclont.
   */
  def copyExtended : AbstractSyntaxTree = {
    var cloned:AbstractSyntaxTree = null
    val toClone:AbstractSyntaxTree = this
    toClone match {
      case toClone:AddExpression =>
        cloned = new AddExpression(toClone.variable, toClone.expression)
      case toClone:Assignment =>
        cloned = new Assignment(toClone.name, toClone.expression)
      case toClone:ConditionEquals =>
        cloned = new ConditionEquals(toClone.variable, toClone.expression)
      case toClone:ConditionGreater =>
        cloned = new ConditionGreater(toClone.variable, toClone.expression)
      case toClone:ConditionGreaterOrEquals =>
        cloned = new ConditionGreaterOrEquals(toClone.variable, toClone.expression)
      case toClone:ConditionLesser =>
        cloned = new ConditionLesser(toClone.variable, toClone.expression)
      case toClone:ConditionLessOrEquals =>
        cloned = new ConditionLessOrEquals(toClone.variable, toClone.expression)
      case toClone:DivExpression =>
        cloned = new DivExpression(toClone.variable, toClone.expression)
      case toClone:IdentExpression =>
        cloned = new IdentExpression(toClone.name)
      case toClone:Ifelse =>
        cloned = new Ifelse(toClone.condition, toClone.thenBranch, toClone.elseBranch)
      case toClone:IntExpression =>
        cloned = new IdentExpression(toClone.name)
      case toClone:MulExpression =>
        cloned = new MulExpression(toClone.variable, toClone.expression)
      case toClone:Program =>
        cloned = new Program(toClone.stmList)
      case toClone:SubExpression =>
        cloned = new SubExpression(toClone.variable, toClone.expression)
      case toClone:WhileStatement =>
        cloned = new WhileStatement(toClone.condition, toClone.doBranch)
    }
      cloned.aeEntry = toClone.aeEntry
      cloned.aeExit = toClone.aeExit
      cloned.allExpressions = toClone.allExpressions
      cloned.blocks = toClone.blocks
      cloned.exitNodes = toClone.exitNodes
      cloned.feature = toClone.feature
      cloned.flow = toClone.flow
      cloned.gen = toClone.gen
      cloned.initNodes = toClone.initNodes
      cloned.kill = toClone.kill
      cloned.number = toClone.number
      cloned.range = toClone.range
    return cloned

  }

    def filterAeEntry(toFilter:List[AbstractSyntaxTree]) {
      var newAeEntry:Set[AbstractSyntaxTree] = Set.empty
      for(stm <- toFilter){
        if(aeEntry.contains(stm)){
          newAeEntry+=stm
        }
      }
      aeEntry=newAeEntry
    }

    def filterAeExit(toFilter:List[AbstractSyntaxTree]) {
      var newAeExit:Set[AbstractSyntaxTree] = Set.empty
      for(stm <- toFilter){
        if(aeExit.contains(stm)){
          newAeExit+=stm
        }
      }
      aeExit=newAeExit
    }

    def filterBlocks(toFilter:List[Opt[AbstractSyntaxTree]]) {
      var newBlocks:Set[AbstractSyntaxTree] = Set.empty
      for(stm <- toFilter){
        if(blocks.contains(stm.entry)){
          newBlocks+=stm.entry
        }
      }
      blocks=newBlocks
    }

    def filterGen(toFilter:List[AbstractSyntaxTree]) {
      var newGen:Set[AbstractSyntaxTree] = Set.empty
      for(stm <- toFilter){
        if(gen.contains(stm)){
          newGen+=stm
        }
      }
      gen=newGen
    }

    def filterKill(toFilter:List[AbstractSyntaxTree]) {
      var newKill:Set[AbstractSyntaxTree] = Set.empty
      for(stm <- toFilter){
        if(kill.contains(stm)){
          newKill+=stm
        }
      }
      kill=newKill
    }

  def setFeaturesTrue { feature=de.fosd.typechef.featureexpr.True  }

  def findNode(statement:AbstractSyntaxTree, stmList:List[Opt[AbstractSyntaxTree]]):Iterator[Opt[AbstractSyntaxTree]] = {
    if(stmList.isEmpty)
      return null
    var iterator:Iterator[Opt[AbstractSyntaxTree]] = stmList.iterator
    var stm:Opt[AbstractSyntaxTree] = stmList.head
    while(!stm.entry.getLabel.equals(statement)){
      if(!iterator.hasNext){
        return null
      }
      stm = iterator.next
      var stmEntry:AbstractSyntaxTree = stm.entry
      stmEntry match{
        case stmEntry:Assignment => {}

        case stmEntry:Ifelse =>
          if(stmEntry.condition.equals(statement))
            return iterator
          val result = findNode(statement, stmEntry.thenBranch.stmList)
          if(result != null)
            return result
          else{
            val  result = findNode(statement, stmEntry.elseBranch.stmList)
            return result
          }

        case stmEntry:WhileStatement =>
          if(stmEntry.condition.equals(statement))
            return iterator
          val result = findNode(statement, stmEntry.doBranch.stmList)
          return result

        case _ => {}
      }
    }
    return iterator
  }

  def nextNode(statement:AbstractSyntaxTree, stmList:List[Opt[AbstractSyntaxTree]]):AbstractSyntaxTree = {
    var iterator:Iterator[Opt[AbstractSyntaxTree]] = findNode(statement, stmList)
    if(iterator==null)
      return null
    var result:AbstractSyntaxTree = null
    var afterStm:Opt[AbstractSyntaxTree] = null

    while(iterator.hasNext){
      afterStm = iterator.next
      afterStm match{
        case afterStm:Opt[Assignment] =>
          if(afterStm.entry.equals(statement))
            return afterStm.entry

        case afterStm:Opt[Ifelse] =>
          result = nextNode(statement, afterStm.entry.thenBranch.stmList)
          if(result != null)
            return result
          else{
            result = nextNode(statement, afterStm.entry.elseBranch.stmList)
            return result
          }

        case afterStm:Opt[WhileStatement] =>
          result = nextNode(statement, afterStm.entry.doBranch.stmList)
          return result
      }
    }
    return result
  }

  def nextNodeSimple(statement:AbstractSyntaxTree, flow:Set[(AbstractSyntaxTree, AbstractSyntaxTree)]):AbstractSyntaxTree = {
    var lastTo:AbstractSyntaxTree = null
    for((from:AbstractSyntaxTree,to:AbstractSyntaxTree)<-flow.toList){
      if(from.equals(statement)){
        if(lastTo==null)
          lastTo=to
        else
          if(to.getPositionFrom.getLine < lastTo.getPositionFrom.getLine)
            lastTo = to
      }

    }
    return lastTo
  }

}