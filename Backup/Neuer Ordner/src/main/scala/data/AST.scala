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


abstract class AbstractSyntaxTree extends WithPosition  {//Opt[AbstractSyntaxTree](FeatureExpr.base, null) with WithPosition{

var initNode : AbstractSyntaxTree = null
var exitNodes : Set[AbstractSyntaxTree] = Set.empty
//var flow : List[(AbstractSyntaxTree,AbstractSyntaxTree)] = List.empty
var flow : Set[(AbstractSyntaxTree,AbstractSyntaxTree)] = Set.empty
var label : AbstractSyntaxTree = null
var aeEntry : Set[AbstractSyntaxTree] = Set.empty
var aeExit : Set[AbstractSyntaxTree] = Set.empty
var gen : Set[AbstractSyntaxTree] = Set.empty
var kill : Set[AbstractSyntaxTree] = Set.empty
var blocks:Set[AbstractSyntaxTree] =Set.empty

var allExpressions:Set[Expression] = Set.empty

def setAllExpressions(set:Set[Expression]){
    aeEntry++=set
    aeExit++=set
}
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

def setInitNode(node:AbstractSyntaxTree) {
        initNode = node
}

def getInitNode:AbstractSyntaxTree = initNode

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
       flow+=((from,to))
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

def equalsString(obj:AbstractSyntaxTree):Boolean ={
  return this.toString.equals(obj.toString)
}

}