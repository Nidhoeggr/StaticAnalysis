package data

import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * Created by IntelliJ IDEA.
 * User:
 * Date: 03.10.11
 * Time: 17:08
 * To change this template use File | Settings | File Templates.
 */

class AddExpression(name: IdentExpression, exp: Expression) extends Expression {
  variable = name
  expression = exp

    override def toString:String = "Add["+variable.toString+","+expression.toString+"]"

}
