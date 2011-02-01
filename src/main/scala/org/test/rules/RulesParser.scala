package org.test.rules

import scala.util.parsing.combinator._

case class Rule(val name: String)

class RulesParser extends JavaTokenParsers {
  def rules: Parser[Map[String,Rule]] = "{" ~> repsep(rule, ",") <~ "}" ^^ { Map() ++ _ }
  def rule: Parser[(String,Rule)] = {
    ruleName ~ "=" ~ ruleValue ^^ {
      case ruleName ~ "=" ~ ruleValue => (ruleName,ruleValue)
    }
  }
  def ruleName: Parser[String] = ident ^^ { case ident => ident }
  def ruleValue: Parser[Rule] = {
    (ident ^^ { case ident => Rule(ident) }) | (stringLiteral ^^ { case stringLiteral => Rule(stringLiteral) })
  }
}

object RulesParser {
  def main(args: Array[String]): Unit = {
    val parser = new RulesParser
    val input = """{
      rule1 = "vv:v1",
      rule2 = "vv:v2"
    }"""
    println(parser.parseAll(parser.rules,input).get)
  }
}

// vim: set ts=2 sw=2 et:
