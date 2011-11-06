package data

import de.fosd.typechef.conditional.{Conditional, Opt}

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:08
 * To change this template use File | Settings | File Templates.
 */

class MulExpression(a: IdentExpression, b: Expression) extends Expression {
    variable = a
    expression = b

    override def toString:String = "Mul["+variable.toString+","+expression.toString+"]"
}
