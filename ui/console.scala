package ui

import values._
import expressions._

class JediException(msg: String = null) extends Exception(msg) {}
class UndefinedException(msg: String = null) extends JediException(msg) {}
class SyntaxException(msg: String = null) extends JediException(msg) {}
//Whats this mean #
//class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error") {}
class TypeException(msg: String = null) extends JediException(msg) {}

object console {
  val parser = new SithParsers()
  val globalEnv = new Environment()

  def execute(cmmd: String): String = {
    val tree = parser.parseAll(parser.expression, cmmd)
    tree match {
      case t: parser.Failure => throw new SyntaxException(t.toString)
      case _ => "" + tree.get.execute(globalEnv)
    }
  }

  def repl {
    var more = true
    while (more) {
      try {
        print("-> ")
        val cmmd = readLine()
        if (cmmd == "quit") {
          println("Goodbye!")
          more = false
        } else {
          println(execute(cmmd))
        }
        // read/execute/print
      } catch {
        case e: SyntaxException => {
          println("Syntax: " + e.getMessage())
        }
        case e: TypeException => {println("Type: " + e.getMessage())}
        case e: UndefinedException => {println("Undefined: " + e.getMessage())}
        // handle other types of exceptions
      } finally {
        Console.flush
      }
    }
  }

  def main(args: Array[String]): Unit = { repl }
}