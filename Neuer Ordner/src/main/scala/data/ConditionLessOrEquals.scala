package data

/**
 * Created by IntelliJ IDEA.
 * User: Familie
 * Date: 03.10.11
 * Time: 17:04
 * To change this template use File | Settings | File Templates.
 */

class ConditionLessOrEquals(a: IdentExpression, b: Expression) extends Condition {
  variable = a
  expression = b

  override def toString:String = "LE["+variable.toString+","+expression.toString+"]"+feature.toString
  override def toStringWithoutFeatures = "LE["+variable.toString+","+expression.toString+"]"
}