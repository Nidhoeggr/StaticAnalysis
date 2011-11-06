package data

import de.fosd.typechef.conditional.{One, Conditional, Opt}

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

  /*
  def printAllNames() {
    for (Opt(f,e)<-b)
      e match {
          e.mapf(
          (f2,stmt)=>stmt match {
          case a:Assignment=>println(a->featureExpr, a.name)

      }

      )

        case One(x) =>
          x match {
            case Assign
          }
      }

  }
 */

//  def printKillGen:String = ""
//  def generateBlocks {}
//  def calculateFlowGraph {}


  def calculateFlowGraph(){
/*
    for(x:Opt[AbstractSyntaxTree]<-stmList){
              x.entry.mapf(
                (feature,entry2) => entry2 match{
                  case entry2:Assignment =>
                    entry2.calculateFlowGraph()
                     addFlow(x.entry.getLabel,entry2.getLabel)        //Kante  vom Opt Knoten zum nächsten Assignment
                  case entry2:Ifelse =>
                    entry2.calculateFlowGraph()
                    addFlow(x.entry.getLabel, entry2.getLabel)        //Kante vom Opt Knoten zur Condition
                  case entry2:WhileStatement =>
                    entry2.calculateFlowGraph()
                    addFlow(x.entry.getLabel, entry2.getLabel)
                  case _ => throw new Exception("Fehler")
                }

              )
      }
*/

    var oldStm:AbstractSyntaxTree = null
   // var oldDefStm:Opt[AbstractSyntaxTree] = null    //zwei old Opt Knoten, da der unäre Operator abweichen kann
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
                 if(oldStm.getExitNodes.isEmpty)
                    addFlow(oldStm,stm.entry.getLabel)
                 for(node<-oldStm.getExitNodes)
                    addFlow(node,stm.entry.getLabel)
               }
               oldStm = stm.entry.getLabel
               addSubFlow(stm.entry.getFlow)
               if(oldDefStmThen!=null){
                 addFlow(oldDefStmThen.entry,stm.entry.getLabel)                    //Kante vom letzten Knoten des Opt-thenBranchs zum nächsten normalen Knoten
                 oldDefStmThen = null
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
              if((oldDefStmThen != null && oldDefStmThen.feature.equivalentTo(stm.feature))){      //Wenn es einen alten Opt Knoten gab und er das selbe Feature hat, dann packe ihn auf den FlowGraph
                addFlow(oldDefStmThen.entry,stm.entry.getLabel)
              }else{                                                        //Falls nicht, kann es eine Kante vom letzten nicht Opt Knoten zum Opt Knoten geben
                if(oldStm != null)
                  addFlow(oldStm, stm.entry.getLabel)
              }
              oldDefStmThen = stm   //.getLabel              //das Label von IfElse ist Condition und damit nicht der Knoten von dem die Kante ausgehen soll
              addSubFlow(stm.entry.getFlow)
            }

            /*
            case de.fosd.typechef.featureexpr.Not(_) =>  {     //Optinaler Knoten
            stm.entry.calculateFlowGraph()
              if((oldDefStm != null && oldDefStm.feature.equals(stm.feature))){      //Wenn es einen alten Opt Knoten gab und er den selben unären Operator hat, dann packe ihn auf den FlowGraph
                addFlow(oldDefStm.entry,stm.entry.getLabel)
              }else{                                                        //Falls nicht, kann es eine Kante vom letzten nicht Opt Knoten zum Opt Knoten geben
                if(oldStm != null)
                  addFlow(oldStm, stm.entry.getLabel)
              }
              oldDefStm = stm   //.getLabel
              addSubFlow(stm.entry.getFlow)
            }
              */

          }

      }
    }
  }

  def calculateExitNodes:Set[AbstractSyntaxTree] = {
    var possibleExits:Set[AbstractSyntaxTree] = exitNodes
    for(stm <- stmList.reverse){
      if(stm.entry.getLabel.feature.equivalentTo(this.getLabel.feature)) {   //TODO label so ubauen, dass es it opt klar kommt
          possibleExits+=stm.entry
          return possibleExits
      } else{
          possibleExits+=stm.entry
      }
  /*
      stm.feature match{
        case de.fosd.typechef.featureexpr.True =>
          possibleExits+=stm.entry
          return possibleExits
        case _ =>
          possibleExits+=stm.entry
      }
  */
  }
    return possibleExits
  }

  def calculateInitNodes:Set[AbstractSyntaxTree] = {
    var possibleInits:Set[AbstractSyntaxTree] = initNodes
    for(stm <- stmList){
      if(stm.entry.getLabel.feature.equivalentTo(this.getLabel.feature)) {   //TODO label so ubauen, dass es it opt klar kommt
          possibleInits+=stm.entry
          return possibleInits
      } else{
          possibleInits+=stm.entry
      }
/*
      stm.feature match{
        case de.fosd.typechef.featureexpr.True =>
          possibleInits+=stm.entry
          return possibleInits
        case _ =>
          possibleInits+=stm.entry
      }
*/
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
/*    var aeExitIntersection:Set[AbstractSyntaxTree]=null
    for((from,to)<-prog.getFlow){
      if(to.equals(this)){
        if(aeExitIntersection == null){
          aeExitIntersection=from.calculateAEexit(prog)
        }else{
          aeExitIntersection=from.calculateAEexit(prog) & aeExitIntersection
        }
      }
    }
    aeExit = aeExitIntersection
    return aeExitIntersection
*/
  }

  override def calculateAEexit(prog:Program):Set[AbstractSyntaxTree] = {
      for(stm<-blocks){
        stm.calculateAEexit(prog)
      }
    return Set.empty
//    return ((aeEntry--kill)++gen)
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

  override def setFeatures {
    for(stm<-stmList){
      stm.entry.setFeatures
      stm.entry.getLabel.feature=stm.feature
    }
  }

}