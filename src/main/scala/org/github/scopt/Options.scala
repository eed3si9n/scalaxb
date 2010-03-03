package org.github.scopt

import collection.mutable.ListBuffer

// Base class for options.
// These are things that get listed when we ask for help,
// and optionally can accept string arguments & perform some kind of action,
// usually mutating a var.
case class OptionDefinition(
        canBeInvoked: Boolean,
        shortopt: String, 
        longopt: String,
        description: String,
        action: String => Unit,
        gobbleNextArgument: Boolean) {

  def shortDescription = "option " + longopt
}

// ----- Some standard option types ---------
class SeparatorDefinition(
        description: String
        ) extends OptionDefinition(false, null, null, description, {a: String => {}}, false)

class Argument(
        name: String,
        description: String,
        action: String => Unit
        ) extends OptionDefinition(false, null, name, description, action, false) {

 override def shortDescription = "argument " + name
}

class ArgOptionDefinition(
        shortopt: String,
        longopt: String,
        description: String,
        action: String => Unit
        ) extends OptionDefinition(true, shortopt, longopt, description, action, true)

class IntArgOptionDefinition(
        shortopt: String,
        longopt: String,
        description: String,
        action: Int => Unit
        ) extends OptionDefinition(true, shortopt, longopt, description, {a: String => action(a.toInt)}, true)

class DoubleArgOptionDefinition(
        shortopt: String,
        longopt: String,
        description: String,
        action: Double => Unit
        ) extends OptionDefinition(true, shortopt, longopt, description, {a: String => action(a.toDouble)}, true)

class BooleanArgOptionDefinition(
        shortopt: String,
        longopt: String,
        description: String,
        action: Boolean => Unit
        ) extends OptionDefinition(true, shortopt, longopt, description, {
  a: String =>
    val boolValue = a.toLowerCase match {
      case "true" => true
      case "false" => false
      case "yes" => true
      case "no" => false
      case "1" => true
      case "0" => false
      case _ => throw new IllegalArgumentException("Expected a string I can interpret as a boolean")
    }
    action(boolValue)
}, true)

class FlagOptionDefinition(
        shortopt: String,
        longopt: String,
        description: String,
        action: => Unit
        )
        extends OptionDefinition(true, shortopt, longopt, description, {a: String => action}, false)


/**
 * OptionParser is instantiated within your object,
 * set up by an (ordered) sequence of invocations of 
 * the various builder methods such as #opt or #arg
 */
case class OptionParser(warnOnUnknownArgument: Boolean) {
  def this() = this (true)

  val options = new ListBuffer[OptionDefinition]
  val arguments = new ListBuffer[Argument]

  // -------- Defining options ---------------
  def add(option: OptionDefinition) {
    option match {
      case a: Argument => arguments += a
      case _ => options += option
    }
  }

  // setup options (which require -char or --string to invoke
  def opt(shortopt: String, longopt: String, description: String, action: String => Unit) =
    add(new ArgOptionDefinition(shortopt, longopt, description, action))

  def opt(shortopt: String, longopt: String, description: String, action: => Unit) =
    add(new FlagOptionDefinition(shortopt, longopt, description, action))

  // we have to give these typed options separate names, because of &^@$! type erasure
  def intOpt(shortopt: String, longopt: String, description: String, action: Int => Unit) =
    add(new IntArgOptionDefinition(shortopt, longopt, description, action))

  def doubleOpt(shortopt: String, longopt: String, description: String, action: Double => Unit) =
    add(new DoubleArgOptionDefinition(shortopt, longopt, description, action))

  def booleanOpt(shortopt: String, longopt: String, description: String, action: Boolean => Unit) =
    add(new BooleanArgOptionDefinition(shortopt, longopt, description, action))

  def help(shortopt: String, longopt: String, description: String = "show this help message") =
    add(new FlagOptionDefinition(shortopt, longopt, description, {this.showUsage; exit}))

  def separator(description: String) =
    add(new SeparatorDefinition(description))

  // regular arguments without the - or -- which have a name purely for help
  def arg(name: String, description: String, action: String => Unit) =
    add(new Argument(name, description, action))


  // -------- Getting usage information ---------------
  def descriptions: Seq[String] = options.map(opt => opt match {
    //case x: Argument => x.longopt + " :\n\t" + opt.description
    case x if !x.canBeInvoked => x.description
    case x if x.gobbleNextArgument => "-" + x.shortopt + " VALUE, --" + x.longopt + " VALUE : " + "\n\t" + x.description
    case _ => "-" + opt.shortopt + ",  --" + opt.longopt + ": " + "\n\t" + opt.description
  }).clone() ++= arguments.map(a => a.longopt + " :\n\t" + a.description)


  def usage: String = {
    val optionText = if (options.isEmpty) {""} else {"[options] "}
    val argumentList = argumentNames.mkString(" ")
    "\nUsage: " + optionText + argumentList + "\n\n" + descriptions.mkString("\n\n") + "\n"
  }

  def showUsage = Console.err.println(usage)

  def argumentNames = arguments.map(_.longopt)

  def applyArgument(option:OptionDefinition, arg:String) :Boolean ={
      try {
        option.action.apply(arg)
        true
      }
      catch {
        case e:NumberFormatException => System.err.println("ERROR: " +
                option.shortDescription + " expects a number but was given '" + arg + "'")
        false
        case e:Throwable => System.err.println("ERROR: " +
                option.shortDescription + " failed when given '" + arg + "'. " + e.getMessage)
        false
      }
  }

  // -------- Parsing ---------------
  def parse(args: Seq[String]): Boolean = {
    var i = 0
    val requiredArgs = arguments.clone
    var answer = true

    while (i < args.length) {
      val arg = args(i)
      val matchingOption = options.find(opt =>
        opt.canBeInvoked
                && (arg == "-" + opt.shortopt || arg == "--" + opt.longopt)
        )

      matchingOption match {
        case None =>
          if (requiredArgs.isEmpty) {
            System.err.println("ERROR: Unknown argument '" + arg + "'")
            answer = false
          }
          else {
            val first = requiredArgs.remove(0)
            if (!applyArgument(first, arg)) {
              answer = false
            }
          }
        case Some(option) =>
          val argToPass = if (option.gobbleNextArgument) {
            i += 1;
            args(i)
          } else
            ""
          if (!applyArgument(option, argToPass)) {
            answer = false
          }
      }
      i += 1
    }

    if (requiredArgs.isEmpty) {
      answer
    }
    else {
      System.err.println("ERROR: missing arguments: " + argumentNames.mkString(", "))
      showUsage
      false
    }
  }
}
