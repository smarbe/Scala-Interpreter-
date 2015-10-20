package values

import expressions._
import ui.UndefinedException

class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {
   def apply(args: List[Value]): Value = {
     var tempEnv = new Environment(defEnv)
     if(args.length != params.length) throw new UndefinedException("Lists are unequal")
     tempEnv.put(params, args)
     body.execute(tempEnv)
   }
   override def toString() = "Closure(" + params + ")"
}