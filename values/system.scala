package values

import expressions._
import ui._

object system {
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)
      case "sub" => sub(args)
      case "mul" => mul(args)
      case "div" => div(args)
      case "equals" => equals(args)
      case "less" => less(args)
      case "not" => not(args)
      case "val" => value(args)
      case "var" => variable(args)
    }
  }
  
  private def value(vals: List[Value]): Value = {
    if(vals.isEmpty) throw new TypeException("Value call expects more than 0 inputs")
    if(vals(0).isInstanceOf[Variable])
      vals(0).asInstanceOf[Variable].content
    else
      throw new TypeException("Value call inputs must be of type Variable")
  }
  
  private def variable(vals: List[Value]): Value = {
    if(vals.isEmpty) throw new TypeException("Variable call expects more than 0 inputs")
    if(vals(0).isInstanceOf[Value])
      new Variable(vals(0).asInstanceOf[Value])
    else
      throw new TypeException("Variable call inputs must be of type Value")
  }
  

  private def add(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("Addition call expects more than 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("Addition call inputs must be of type Number")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ + _)
  }

  private def sub(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("Subtraction call expects more than 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("Subtraction call inputs must be of type Number")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ - _)
  }

  private def mul(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("Multiplication call expects more than 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("Multiplication call inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ * _)
  }

  private def div(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("Division call expects more than 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("Division call inputs must be of type Number")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ / _)
  }

  private def equals(vals: List[Value]): Boole = {
    if (vals.isEmpty) throw new TypeException("Equal call expects more than 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("Equal call inputs must be of type Number")
    val args2 = vals.map(_.asInstanceOf[Number])
    var prev = args2.head
    var retval = true
    for(a <- args2.tail){
      if(!(prev == a).value)
        retval = false
      prev = a
    }
    new Boole(retval)
  }

  private def less(vals: List[Value]): Boole = {
    if (vals.isEmpty) throw new TypeException("Less call expects more than 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("Less call inputs must be of type Number")
    val args2 = vals.map(_.asInstanceOf[Number])
    var prev = args2.head
    var retval = true
    for(a <- args2.tail){
      if((prev > a).value)
        retval = false
      prev = a
    }
    new Boole(retval)
  }

  private def not(vals: List[Value]): Boole = {
	if (vals.isEmpty) throw new TypeException("Not call expects more than 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Boole])
    if (ok.length < vals.length) throw new TypeException("Not call inputs must be of type Boole")
	if(vals.length > 1) throw new TypeException("Not call can only take 1 input")
    val args2 = vals.map(_.asInstanceOf[Boole])
    var retval: Boole = vals.head.asInstanceOf[Boole]
    retval.!
  }
}