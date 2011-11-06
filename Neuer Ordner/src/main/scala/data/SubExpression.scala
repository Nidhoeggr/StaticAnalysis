package data

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:08
 * To change this template use File | Settings | File Templates.
 */

class SubExpression(a: IdentExpression, b: Expression) extends Expression {
  variable = a
  expression = b

    override def toString:String = "Sub["+variable.toString+","+expression.toString+"]"
}
