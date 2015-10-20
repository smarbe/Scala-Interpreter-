package expressions

import values._
import ui.TypeException
import ui.UndefinedException

case class FunCall(operators: Expression = null, operands: List[Expression] = null) extends Expression {
  def execute(env: Environment): Value = {
    var passOperand: List[Value] = Nil
    for (op <- operands) {
      passOperand = passOperand :+ op.execute(env)
    }
    try {
      val temp = operators.execute(env)
      if(!temp.isInstanceOf[Closure]) throw new TypeException("Not of type closure")
      val temps = temp.asInstanceOf[Closure]
      temps(passOperand)
    } catch {
      case e: UndefinedException => system.execute(operators.asInstanceOf[Identifier], passOperand)
    }
  }
}