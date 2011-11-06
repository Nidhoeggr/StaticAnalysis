package data

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:03
 * To change this template use File | Settings | File Templates.
 */

class ConditionLesser(a: IdentExpression, b: Expression) extends Condition {
  variable = a
  expression = b

  override def toString:String = "Le["+variable.toString+","+expression.toString+"]"+feature.toString
}
