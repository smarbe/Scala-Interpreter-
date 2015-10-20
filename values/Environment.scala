package values

import scala.collection.mutable.HashMap
import expressions._
import ui.UndefinedException

class Environment(nextEnv: Environment = null) extends HashMap[Identifier, Value] with Value {
	def put(idList: List[Identifier], valList: List[Value]) = {
		if(idList.length != valList.length)
		  throw new Exception //Change exception later
		else{
		  for(i <- 0 until idList.length)
			  super.put(idList(i), valList(i))
		  }
	}
	
	def find(key: Identifier): Value = {
	  if(this.contains(key))
		this(key)
	  else if(this.nextEnv != null)
	    this.nextEnv.find(key)
	  else
	    throw new UndefinedException("Undefined Identifier: " + key)
	}
}