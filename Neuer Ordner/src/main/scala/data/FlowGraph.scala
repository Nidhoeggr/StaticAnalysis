package data

import de.fosd.typechef.conditional.Opt


/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 21.09.11
 * Time: 15:42
 * To change this template use File | Settings | File Templates.
 */

class FlowGraph() {
 /*
//val ast:AbstractSyntaxTree = a
val initLabel:Int = 1
var exitLabels = Set.empty[Int]
var partExit = Set.empty[Int]
var interFlowGraph = List.empty[(Int,Int)]

  def add(nextDirectLabel: Int){
    interFlowGraph::=(initLabel,nextDirectLabel)
  }

  def addEdge(label:Int, nextLabel:Int){
    interFlowGraph::=(label,nextLabel)
  }

  def getInitLabel : Int = this.initLabel
  def getExitLabels : Set[Int]= exitLabels
  def getInterFlowGraph = interFlowGraph

  def calculateFlowGraph(stm:AbstractSyntaxTree) {
        stm match{
          case Ifelse(c,t,e) =>
                              generateBlocks(stm)

                              stm.setLabel(stm.getPositionFrom.getLine)
                              addEdge(stm.getLabel, t.b.head.entry.getPositionFrom.getLine)
                              stm.addFlow(stm.getLabel, t.b.head.entry.getPositionFrom.getLine)
                              addEdge(stm.getLabel, e.b.head.entry.getPositionFrom.getLine)    //Kante von der Condition zum ersten Eintrag des Elsebranchs
                              stm.addFlow(stm.getLabel, e.b.head.entry.getPositionFrom.getLine)
                              addEdge(t.b.last.entry.getPositionFrom.getLine, e.b.last.entry.getPositionTo.getLine)     //Kante vom letzten Eintrag des Ifbranchs zum ersten Eintrag nach If Else
                              stm.addFlow(t.b.last.entry.getPositionFrom.getLine, e.b.last.entry.getPositionTo.getLine)
                              stm.setInitNode(stm.getLabel)
                              t.setInitNode(stm.getLabel)
                              e.setInitNode(stm.getLabel)
                              stm.addExitNode(t.b.last.entry.getPositionFrom.getLine)    //final(ifelse) =
                              stm.addExitNode(e.b.last.entry.getPositionFrom.getLine)   // final(if) U final(else)
                              calculateFlowGraphFromList(t);   //Kanten des Ifbranchs berechnen
                              calculateFlowGraphFromList(e);   //Kanten des ElseBranchs berechnen
                              stm.addSubFlow(t.getFlow)
                              stm.addSubFlow(e.getFlow)
//          case IfBranch(b) =>
//                              stm.setLabel(stm.getPositionFrom.getLine)
//                              addEdge(stm.getLabel, b.b.head.entry.getPositionTo.getLine)   //Kante zum nächsten Eintrag
//                              calculateFlowGraphFromList(b.b)
//                              stm.addExitNode(b.b.last.entry.getPositionFrom.getLine)
//          case ElseBranch(b) =>
//                               stm.setLabel(stm.getPositionFrom.getLine)
//                               addEdge(stm.getLabel, b.b.head.entry.getPositionTo.getLine)   //Kante zum nächsten Eintrag
//                               calculateFlowGraphFromList(b.b)
//                               stm.addExitNode(b.b.last.entry.getPositionFrom.getLine)
          case WhileStatement(c,b) =>
                               generateBlocks(stm)

                               stm.setLabel(stm.getPositionFrom.getLine)      //label des gesamten While-Statements
                               stm.setInitNode(stm.getLabel)     //init der condition
                               b.setInitNode(stm.getLabel)     //init des do-block
                               addEdge(stm.getLabel, b.b.last.entry.getPositionTo.getLine)     //Kante zum ersten Eintrag nach der SChleife
                               stm.addFlow(stm.getLabel,b.b.last.entry.getPositionTo.getLine)
                               addEdge(stm.getLabel, b.b.head.entry.getPositionFrom.getLine)   //Kante zum nächsten Eintrag
                               stm.addFlow(stm.getLabel,b.b.head.entry.getPositionFrom.getLine)
                               stm.addExitNode(stm.getLabel) //final(while) = label(condition)
                               addEdge(b.b.last.entry.getPositionFrom.getLine, stm.getLabel)               //Kante zur Condition
                               stm.addFlow(b.b.last.entry.getPositionFrom.getLine, stm.getLabel)
                               calculateFlowGraphFromList(b)
                               stm.addSubFlow(b.getFlow)
          case Assignment(a,b) =>
                               generateBlocks(stm)

                               stm.setLabel(stm.getPositionFrom.getLine)
                               stm.setInitNode(stm.getLabel)
                               b.setInitNode(stm.getLabel)
                               addEdge(stm.getLabel, stm.getPositionTo.getLine)   //Kante zum nächsten Eintrag
                               stm.addFlow(stm.getLabel, stm.getPositionTo.getLine)
                               stm.addExitNode(stm.getLabel)       //final(assignment) = label(assignment)
          case _ =>
        }
      }
/*
  def calculateFlowGraphFromList(stm: List[Opt[AbstractSyntaxTree]]){
      partExit+=(stm.last.entry.getPositionTo.getLine)
      for(Opt(_,entry)<-stm){
        entry match{
          case Ifelse(c,t,e) =>
                               calculateFlowGraph(entry)
          case WhileStatement(c,b) =>
                               calculateFlowGraph(entry)
          case Program(b) =>
                               entry.setLabel(entry.getPositionFrom.getLine)
                               if(entry.getInitNode != -1){
                                 entry.setInitNode(entry.getLabel)
                               }
                               entry.addFlow(entry.getLabel, b.head.entry.getPositionTo.getLine)
                               addEdge(entry.getLabel, b.head.entry.getPositionTo.getLine)   //Kante zum nächsten Eintrag
                               calculateFlowGraphFromList(b)
                               for(Opt(_,tmp)<-b){
                                 entry.addSubFlow(tmp.getFlow)
                               }
                               entry.addExitNode(b.last.entry.getPositionFrom.getLine)    //final(S1,S2) = final(S2)
          case Assignment(a,b) =>
                               entry.setLabel(entry.getPositionFrom.getLine)
                               entry.setInitNode(entry.getLabel)
                               if(!partExit.exists(x => x == entry.getPositionTo.getLine))            //Letzte Zuweisung eines Abschnitts darf keinen Nachfolger haben (Ausnahme While oben definiert)
                                calculateFlowGraph(entry)
          case _ =>
        }

      }
  }
*/
  def calculateFlowGraphFromList(stm: Program){


      partExit+=(stm.b.last.entry.getPositionTo.getLine)
      for(Opt(_,entry)<-stm.b){
        entry match{
          case Ifelse(c,t,e) =>
                               calculateFlowGraph(entry)
          case WhileStatement(c,b) =>
                               calculateFlowGraph(entry)

          case Assignment(a,b) =>
                               entry.setLabel(entry.getPositionFrom.getLine)
                               entry.setInitNode(entry.getLabel)
                               if(!partExit.exists(x => x == entry.getPositionTo.getLine))            //Letzte Zuweisung eines Abschnitts darf keinen Nachfolger haben (Ausnahme While oben definiert)
                                calculateFlowGraph(entry)
          case _ =>
        }
      }
        if(stm.getInitNode != -1){
          stm.setLabel(stm.getInitNode)
      //  stm.addFlow(stm.getLabel, stm.b.head.entry.getPositionTo.getLine)
     //     addEdge(stm.getLabel, stm.b.head.entry.getPositionTo.getLine)   //Kante zum nächsten Eintrag
          for(tmp<-stm.b){
            stm.addSubFlow(tmp.entry.getFlow)
          }
          stm.addExitNode(stm.b.last.entry.getPositionFrom.getLine)    //final(S1,S2) = final(S2)
    }

  }


  def generateBlocks(stm:AbstractSyntaxTree){
    stm match{
      case Ifelse(i,t,e) => {
        generateBlocks(i)
        for(Opt(_,tmp)<-t.b){
          generateBlocks(tmp)
          t.addBlocks(tmp)
        }
        for(Opt(_,tmp)<-e.b){
          generateBlocks(tmp)
          e.addBlocks(tmp)
        }
      stm.addBlocks(i)
      stm.addBlocks(t)
      stm.addBlocks(e)
      }

      case WhileStatement(c,p) => {
        generateBlocks(c)
        stm.addBlocks(c)
        for(Opt(_,tmp)<-p.b){
          generateBlocks(tmp)
          p.addBlocks(tmp)
        }
        stm.addBlocks(p)
      }

      case Assignment(a,b) => {
        generateBlocks(b)
        stm.addBlocks(b)
        stm.addBlocks(stm)
      }

      case AddExpression(a,b)    =>
            generateBlocks(b)
            stm.addBlocks(b)
            stm.addBlocks(stm)


      case  SubExpression(a,b)  =>
            generateBlocks(b)
            stm.addBlocks(b)
            stm.addBlocks(stm)

      case  MulExpression(a,b)  =>
            generateBlocks(b)
            stm.addBlocks(b)
            stm.addBlocks(stm)

      case  DivExpression(a,b)     =>
            generateBlocks(b)
            stm.addBlocks(b)
            stm.addBlocks(stm)


      case ConditionEquals(a,b)   => {
        generateBlocks(b)
        stm.addBlocks(b)
        stm.addBlocks(stm)
      }
      case  ConditionGreater(a,b)  => {
        generateBlocks(b)
        stm.addBlocks(b)
        stm.addBlocks(stm)
      }
      case  ConditionLesser(a,b)   => {
        generateBlocks(b)
        stm.addBlocks(b)
        stm.addBlocks(stm)
      }
      case  ConditionGreaterOrEquals(a,b)  => {
        generateBlocks(b)
        stm.addBlocks(b)
        stm.addBlocks(stm)
      }
      case  ConditionLessOrEquals(a,b)  => {
        generateBlocks(b)
        stm.addBlocks(b)
        stm.addBlocks(stm)
      }

      case _ =>
    }
  }



 */
}