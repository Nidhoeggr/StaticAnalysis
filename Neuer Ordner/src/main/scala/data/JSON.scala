package data

import scala.util.parsing.combinator._

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 05.09.11
 * Time: 14:14
 * To change this template use File | Settings | File Templates.
 */

class JSON extends JavaTokenParsers {
/*
  def start : Parser[Map[String,Object]] =
              "begin"~"{"~>program<~"}"~"end"

  def program : Parser[Map[String,Object]] =
                rep(statement) ^^ {Map() ++ _}

  def statement : Parser[(String,Object)] =
                assignment    |
                loop          |
                ifelse        |
                skip

  def assignment : Parser[(String,Object)] =
                decimalNumber~":"~identifier~"="~expression  ^^ {case dn~":"~name~"="~value => (dn, new AssignmentToken(name+"="+value))}

  def loop : Parser[(String,Object)] =
                decimalNumber~":"~"while("~condition~")"~"{"~program~"}" ^^ {case dn~":"~"while("~con~")"~"{"~prog~"}" =>(dn,new WhileToken(con+" loop: {"+prog+"}"))}

  def ifelse : Parser[(String,Object)] =
                decimalNumber~":"~"if("~condition~"){"~program~"}"  ^^{case dn~":"~"if("~con~"){"~prog~"}"  => (dn, new IfToken("("+con+"){"+prog+"}"))}   |
                "else{"~program~"}"   ^^{case "else{"~prog~"}" =>("0", new ElseToken("{"+prog+"}"))}

  def condition : Parser[Object] =
                (identifier | decimalNumber)~"=="~expression    ^^ {case id~"=="~exp => new ConditionToken(id+"=="+exp)} |
                (identifier | decimalNumber)~">"~expression     ^^ {case id~">"~exp => new ConditionToken(id+">"+exp)}   |
                (identifier | decimalNumber)~">="~expression    ^^ {case id~">="~exp => new ConditionToken(id+">="+exp)} |
                (identifier | decimalNumber)~"<="~expression    ^^ {case id~"<="~exp => new ConditionToken(id+"<="+exp)} |
                (identifier | decimalNumber)~"<"~expression     ^^ {case id~"<"~exp => new ConditionToken(id+"<"+exp)}   |
                "!"~expression                                  ^^ {case "!"~exp => new ConditionToken("!"+exp)}         |
                "false"                                                                                |
                "true"

  def expression : Parser[Object] =
                (identifier | decimalNumber)~"+"~expression  ^^ {case exp~"+"~exp2 => new ExpressionToken(exp+"+"+exp2)} |
                (identifier | decimalNumber)~"-"~expression  ^^ {case exp~"-"~exp2 => new ExpressionToken(exp+"-"+exp2)} |
                (identifier | decimalNumber)~"*"~expression  ^^ {case exp~"*"~exp2 => new ExpressionToken(exp+"*"+exp2)} |
                decimalNumber  |
                identifier

  def identifier : Parser[String] =
                ident

  def skip: Parser[(String,Object)] =
                decimalNumber~":"~"skip"  ^^ {case dn~":"~skip  => (dn,new SkipToken("skip"))}
  */
}

