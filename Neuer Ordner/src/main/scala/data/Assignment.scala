package data

import de.fosd.typechef.parser.java15.TokenWrapper
import de.fosd.typechef.conditional.{One, Choice, Conditional, Opt}

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:10
 * To change this template use File | Settings | File Templates.
 */

class Assignment(n: IdentExpression, exp: Expression) extends Statement {
var name:IdentExpression = n
var expression:Expression = exp

  this.setLabel(this)



  def calculateFlowGraph() {
              //                  generateBlocks(stm)
                               addInitNode(getLabel)
                               expression.addInitNode(getLabel)
//                               if(label!=exitLabel)
//                                  addFlow(getLabel, getPositionTo.getLine)
                               addExitNode(getLabel)
}

  def generateBlocks {
    addBlocks(this)
  }

override def killAE(caller:AbstractSyntaxTree) {
  for(exp<-caller.allExpressions){
    if(exp.containsVariable(name)){
      kill+=exp
    }
  }
}

  override def genAE{
    if(!expression.getExpressions.isEmpty){
      for(exp<-expression.getExpressions)
       if(!exp.containsVariable(name)){
          gen+=exp
      }
    }else{
       if(!expression.containsVariable(name)){
          gen+=expression
       }
    }
  }

  /*
  Alle AE(Statement1) die zu diesem Statement eine Kante haben und das gleiche Feature werden geschnitten, der Rest mit unterschiedlichen
  Features wird zu dieser Menge gejoint.
   */
  override def calculateAEentry(prog:Program):Set[AbstractSyntaxTree] ={
    var aeExitIntersection:Set[AbstractSyntaxTree]= Set.empty
    var aeExitIntersectionDifferentFeature:Set[AbstractSyntaxTree]=null
    var aeExitIntersectionSameFeature:Set[AbstractSyntaxTree]=null
//    var tmpSet:Set[AbstractSyntaxTree]=Set.empty
    for((from,to)<-prog.getFlow){
      if(to.equals(this)){
          if(from.getLabel.feature.equivalentTo(to.getLabel.feature)){
            if(aeExitIntersectionSameFeature == null){
              aeExitIntersectionSameFeature = from.calculateAEexit(prog)
            }else{
              aeExitIntersectionSameFeature = from.calculateAEexit(prog) & aeExitIntersectionSameFeature
            }
          }else{    //Features stimmen nicht überein, daher ist ein unterschiedlicher Opt Knoten gegeben und es muß ein join durchgeführt werden
            if(aeExitIntersectionDifferentFeature == null){
              aeExitIntersectionDifferentFeature = from.calculateAEexit(prog)
            }else{
              aeExitIntersectionDifferentFeature = from.calculateAEexit(prog) ++ aeExitIntersectionDifferentFeature
            }
          }
      }
    }
    if(aeExitIntersectionSameFeature != null){
      if(aeExitIntersectionDifferentFeature != null){
        aeExitIntersection = aeExitIntersectionDifferentFeature ++ aeExitIntersectionSameFeature
      }else{
        aeExitIntersection = aeExitIntersectionSameFeature
      }
    }else{
      if(aeExitIntersectionDifferentFeature != null){
        aeExitIntersection = aeExitIntersectionDifferentFeature
      }
    }
    aeEntry = aeExitIntersection
    return aeExitIntersection
  }

  override def calculateAEexit(prog:Program):Set[AbstractSyntaxTree] = {
    var aeEntryKill:Set[AbstractSyntaxTree] = aeEntry--kill
    var aeEntryUnionGen:Set[AbstractSyntaxTree] = aeEntryKill ++ gen
    aeExit = aeEntryUnionGen
    return aeEntryUnionGen
  }

  override def toString:String = "Ass["+name.toString+","+exp.toString+"]"+feature.toString

  override def printKillGen:String = "\n"+getLabel+"Kill: "+kill.toString+"Gen: "+gen.toString

  override def printAE:String = "\n"+getLabel+" AEentry: "+aeEntry.toString+" AEexit: "+aeExit.toString

    override def generateAllExpressions{
      expression match {
        case expression:IdentExpression => allExpressions+=expression
        case expression:IntExpression => allExpressions+=expression
        case _ =>
          expression.generateAllExpressions
          allExpressions++=expression.getExpressions
      }
  }

}