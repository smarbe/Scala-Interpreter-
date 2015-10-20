package expressions

import values._
import ui.TypeException

case class Conditional(condition: Expression, consequent: Expression, alternate: Expression = null) extends SpecialForm {
  def execute(env: Environment): Value = {
	val r1 = condition.execute(env)
	if(!r1.isInstanceOf[Boole]) throw new TypeException("Conditional must be of type Boole")
    if (r1.asInstanceOf[Boole].value){
      consequent.execute(env)
    }
    else if(alternate.equals(null))
      Notification.UNKNOWN
    else
      alternate.execute(env)
  }
}