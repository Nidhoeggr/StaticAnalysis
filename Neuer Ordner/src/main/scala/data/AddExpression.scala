package data

import de.fosd.typechef.conditional.{Conditional, Opt}

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:08
 * To change this template use File | Settings | File Templates.
 */

class AddExpression(name: IdentExpression, exp: Expression) extends Expression {
  variable = name
  expression = exp

    override def toString:String = "Add["+variable.toString+","+expression.toString+"]"
}
