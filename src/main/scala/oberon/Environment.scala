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
  private val stack = Stack[Map[String, Value]]()
  private val stackProcedure =  Stack[Map[String,Procedure]]()

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

  def pushProcedure(): Unit = {
    stackProcedure.push(new HashMap[String, Procedure]())
  }

  def popProcedure(): Unit = {
    stackProcedure.pop()
  }

  def mapProcedure(id: String, value: Procedure): Unit = {
    if(stackProcedure.isEmpty) {
      pushProcedure()
    }
    stackProcedure.top += (id -> value)
  }

  def lookupProcedure(id: String): Option[Procedure] =
    if (stackProcedure.isEmpty) None else Some(stackProcedure.top(id))

  def clearProcedure(): Unit = {
    stackProcedure.clear()
  }

}