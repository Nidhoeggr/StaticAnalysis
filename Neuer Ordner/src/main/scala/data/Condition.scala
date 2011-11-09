package data

import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:00
 * To change this template use File | Settings | File Templates.
 */

abstract class Condition() extends Statement {
  var variable:IdentExpression = null
  var expression:Expression = null
  setLabel(this)

   def calculateFlowGraph(){
                               addInitNode(this)
                               expression.addInitNode(this)
                              if(getExitNodes.isEmpty)
                                addExitNode(this)
   }

  def generateBlocks {
    addBlocks(this)
  }

  override def genAE{
    gen+=expression
  }

  override def printKillGen:String =  "\n"+getLabel+"Kill: "+kill.toString+"Gen: "+gen.toString

  override def printAE:String = "\n"+getLabel+" AEentry: "+aeEntry.toString+" AEexit: "+aeExit.toString

    override def generateAllExpressions{
        allExpressions+=expression
  }

  override def calculateAEentry(prog:Program):Set[AbstractSyntaxTree] ={
    var aeExitIntersection:Set[AbstractSyntaxTree]=null
    var aeExitIntersectionTmpOne:Set[AbstractSyntaxTree]=Set.empty
    for((from,to)<-prog.getFlow){
      if(to.equals(this)){
        if(aeExitIntersection == null){
          aeExitIntersection=from.calculateAEexit(prog)
        }else{
          //aeExitIntersection= from.calculateAEexit(prog) & aeExitIntersection
          aeExitIntersectionTmpOne = from.calculateAEexit(prog)
          var tmpSet:Set[AbstractSyntaxTree]=Set.empty
          for(tmp1 <-aeExitIntersectionTmpOne)                                     //equals selber simulieren, da ich auf textuelle Gleichheit prüfen muß
            for(tmp2 <-aeExitIntersection){
              if(tmp1.toString.equals(tmp2.toString)){
                tmpSet+=tmp1
              }
            }
          aeExitIntersection = tmpSet
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

    override def setFeatures(feature:FeatureExpr){
      variable.setFeatures(feature)
      expression.setFeatures(feature)
      this.label.feature = feature
    }

}