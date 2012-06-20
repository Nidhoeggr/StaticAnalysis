package data

import de.fosd.typechef.featureexpr.FeatureExpr
import util.Random
import de.fosd.typechef.lexer.Feature

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 16.06.12
 * Time: 13:39
 * To change this template use File | Settings | File Templates.
 */

class Generator(deepness : Int, numberVariables: Int,  numberFeatures:Int, numberBlocks: Int) {
var variables:Array[String] = Array("a","b","c","d","e","f","g","h","i","j","k","l","m","n")    //nur 14 mögliche features, da die laufzeit ab 12 features (für die brute-force) explodiert
var usedFeatures:Set[String] = Set.empty


  def generateProgramm(feature:String, deep:Int):String = {
    var code:String = ""
    var block : AbstractSyntaxTree = null;
    code+= " //#if " + feature + " \n "
    if(deep>=deepness || deep >= numberBlocks){
        code += " " + getVariable() + " = " + generateExpression() + " ; "
        code += " //#endif \n "
        return code
    }
    getRandom(4) match {
      case 0 =>                                          //Assignment
        code += " " + getVariable() + " = " + generateExpression() + " ; "
        code += " //#endif \n "
      case 1 =>                                          //IfElse
        var thenB:String = generateProgramm(generateValidFeature(feature), deep+1)
        var elseB:String = generateProgramm(generateValidFeature(feature), deep+1)
        code += " if( " + generateCondition() + " ){ " + thenB +"} else { " + elseB + " } "
        code += " //#endif \n "
      case 2 =>                                             //while
        var cond:String = generateProgramm(generateValidFeature(feature), deep+1)
        code += " while( " + generateCondition() + " ){ " + cond + " } "
        code += " //#endif \n "
      case 3 =>                                             //sequenz
        var sequenz1:String = generateProgramm(generateValidFeature(feature), deep+1)
        var sequenz2:String = generateProgramm(generateValidFeature(feature), deep+1)
        code += " " + sequenz1 + " " + sequenz2 + " "
        code += " //#endif \n "
    }
    return code
  }

  def generateValidFeature(feature:String):String = {
    if(usedFeatures.size>=numberFeatures || (getRandom(4)+1)%2==0)
      return "true"
    var newFeature:String = getVariable()
    if(usedFeatures.contains(newFeature)){
      return feature
    }
    else{
      usedFeatures+=newFeature
      return newFeature
    }
    return feature
  }

  def getVariable():String = {
      return variables(getRandom(numberVariables))
  }

  def generateCondition():String = {
    getRandom(5) match {
      case 0 =>
        return getVariable() + " == " + getRandom(101).toString()
      case 1 =>
        return getVariable() + " > " + getRandom(101).toString()
      case 2 =>
        return getVariable() + " >= " + getRandom(101).toString()
      case 3 =>
        return getVariable() + " < " + getRandom(101).toString()
      case 4 =>
        return getVariable() + " <= " + getRandom(101).toString()
    }
  }

  def generateExpression():String = {
    getRandom(6) match {
      case 0 =>
        return getVariable() + " + " + getRandom(101).toString()
      case 1 =>
        return getVariable() + " / " + getRandom(101).toString()
      case 2 =>
        return getVariable() + " * " + getRandom(101).toString()
      case 3 =>
        return getVariable() + " - " + getRandom(101).toString()
      case 4 =>
        return getVariable()
      case 5 =>
        return getRandom(101).toString()
    }
  }

  def getRandom(range:Int):Int = {
       Random.nextInt(range);
  }



}