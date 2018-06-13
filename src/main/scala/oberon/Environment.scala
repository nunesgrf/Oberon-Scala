package oberon

import java.rmi.activation.ActivationGroupDesc.CommandEnvironment

import oberon.command._

import scala.collection.mutable.Stack
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import oberon.expression.Value
import oberon.expression._

import scala.collection.mutable

object Environment {
  var stack = new Stack[Map[String, Value]]()
  var hashProcedure = new HashMap[String, List[Variable]]
  var stackProcedure = new Stack[List[Command]]

  def push() {
    stack.push(new HashMap[String, Value]())
  }

  def pop() {
    stack.pop()
  }

  def map(id: String, value: Value) {
    if (stack.isEmpty) {
      push()
    }
    stack.top += (id -> value)
  }

  def lookup(id: String): Option[Value] =
    if (stack.isEmpty) None else Some(stack.top(id))

  def clear(): Unit = {
    stack.clear()
  }

}