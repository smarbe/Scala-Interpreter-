package expressions

import values._
import ui.TypeException

case class Object(exps: List[Expression], extend: Identifier = null) extends SpecialForm {
  def execute(env: Environment): Value = {
    if (extend == null) {
      val curEnv = new Environment(env)
      for (i <- exps) {
        i.execute(curEnv)
      }
      curEnv
    } else {
      if (extend.execute(env).isInstanceOf[Environment]) {
        val curEnv = new Environment(extend.execute(env).asInstanceOf[Environment])
        for (i <- exps) {
          i.execute(curEnv)
        }
        curEnv
      } else
        throw new TypeException("Identifier must be of type Environment")
    }
  }
}