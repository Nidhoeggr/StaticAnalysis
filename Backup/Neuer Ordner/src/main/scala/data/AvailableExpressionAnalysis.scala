package data

import de.fosd.typechef.conditional.Opt


/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 02.10.11
 * Time: 19:24
 * To change this template use File | Settings | File Templates.
 */

class AvailableExpressionAnalysis {
/*
  def aeExpressionInit(prog:Program, flow:FlowGraph){
    for(Opt(_,value)<-prog.b){
       calculateEntry(value,flow,prog)
       calculateExit(value,flow,prog)
    }
  }

  def availableExpression(stm : AbstractSyntaxTree, flow : FlowGraph, prog : Program) {
               stm match{
          case Ifelse(c,t,e) =>
                //if stm.getLabel == 1 noch hinzufügen
                calculateEntry(stm,flow,prog)
                calculateExit(stm,flow,prog)
          case WhileStatement(c,b) =>

          case Assignment(a,b) =>

          case _ =>
        }
  }

  def calculateEntry(stm:AbstractSyntaxTree, flow: FlowGraph, prog:Program){
    var intersectionExit : Set[AbstractSyntaxTree] = Set.empty  //Schnittmenge aller ExitNodes, die im Flußgraphen auf stm zeigen
     var tmpExit : Set[AbstractSyntaxTree] = Set.empty
     for((from,to)<-flow.getInterFlowGraph){   //flow Graph durchlaufen
       if(to==stm.getLabel){                   //alle Labels suchen, die auf unser Statement zeigen
          var tmp:AbstractSyntaxTree=findStmByLabel(from, prog)             //nun das Statement zu dem Label suchen (das muß man optimieren)
         if(tmp!=null){
             if(tmpExit.isEmpty){
               intersectionExit=tmp.getEntry & tmpExit
               tmpExit=Set.empty
             }else{
               tmpExit = tmp.getEntry
             }
         }
       }
     }
     stm.setEntry(intersectionExit)
  }

  def calculateExit(stm:AbstractSyntaxTree, flow: FlowGraph, prog:Program){
    var aeEntry : Set[AbstractSyntaxTree] = stm.getEntry
    var killAE : Set[AbstractSyntaxTree] = Set.empty
    var genAE : Set[AbstractSyntaxTree] = Set.empty
    for(block<-stm.getBlocks){
        if(killSet(block)){
          killAE+=block
        }
        if(genSet(block)){
          genAE+=block
        }
    }
    var aeExit = (aeEntry--killAE)++genAE
  }

  def killSet(block:AbstractSyntaxTree) : Boolean = {
        block match{
          case Assignment(a,b) =>
                  if(b.toString.contains(a)){      //Vereinfachte Annahme, dass variablen nur aus einem Buchstaben bestehen
                    return true
                  }
          case _ =>
        }
        return false
  }

  def genSet(block:AbstractSyntaxTree) : Boolean = {
        block match{
          case Assignment(a,b) =>
            b match{
              case SubExpression(x,y) =>
                  if(b.toString.contains(a)){      //Vereinfachte Annahme, dass variablen nur aus einem Buchstaben bestehen. Außerdem wird nur die gesamte Expression erzeugt
                    return false
                  }
              case MulExpression(x,y) =>
                  if(b.toString.contains(a)){      //Vereinfachte Annahme, dass variablen nur aus einem Buchstaben bestehen. Außerdem wird nur die gesamte Expression erzeugt
                    return false
                  }
              case AddExpression(x,y) =>
                  if(b.toString.contains(a)){      //Vereinfachte Annahme, dass variablen nur aus einem Buchstaben bestehen. Außerdem wird nur die gesamte Expression erzeugt
                    return false
                  }
              case DivExpression(x,y) =>
                  if(b.toString.contains(a)){      //Vereinfachte Annahme, dass variablen nur aus einem Buchstaben bestehen. Außerdem wird nur die gesamte Expression erzeugt
                    return false
                  }
              case _ =>
            }
          case _ =>
            //TODO andere Blöcke werden bisher nicht beachtet
        }
    return true
  }

  def findStmByLabel(label:Int, prog:Program) : AbstractSyntaxTree = {
    for(block<-prog.getBlocks){
      if(block.getLabel==label)
          return block
    }
    return null
  }
*/
}