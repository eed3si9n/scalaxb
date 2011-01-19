package org.github.scopt

import collection.mutable.ListBuffer

// Base class for options.
// These are things that get listed when we ask for help,
// and optionally can accept string arguments & perform some kind of action,
// usually mutating a var.
case class OptionDefinition(
        canBeInvoked: Boolean,
        shortopt: Option[String],
        longopt: String,
        keyName: String,
        valueName: String,
        description: String,
        action: String => Unit,
        gobbleNextArgument: Boolean,
        keyValueArgument: Boolean) {
  def shortDescription = "option " + longopt
}

// ----- Some standard option types ---------
class SeparatorDefinition(
        description: String
        ) extends OptionDefinition(false, null, null, null, null,
          description, {a: String => {}}, false, false)

class Argument(
        name: String,
        description: String,
        val allowMultiple: Boolean,
        action: String => Unit
        ) extends OptionDefinition(false, null, name, null, name,
          description, action, false, false) {

  override def shortDescription = "argument " + name
}

class ArgOptionDefinition(
        shortopt: Option[String],
        longopt: String,
        valueName: String,
        description: String,
        action: String => Unit
        ) extends OptionDefinition(true, shortopt, longopt, null, valueName,
          description, action, true, false)

class IntArgOptionDefinition(
        shortopt: Option[String],
        longopt: String,
        valueName: String,
        description: String,
        action: Int => Unit
        ) extends OptionDefinition(true, shortopt, longopt, null, valueName,
          description, {a: String => action(a.toInt)}, true, false)

class DoubleArgOptionDefinition(
        shortopt: Option[String],
        longopt: String,
        valueName: String,
        description: String,
        action: Double => Unit
        ) extends OptionDefinition(true, shortopt, longopt, null, valueName,
          description, {a: String => action(a.toDouble)}, true, false)

class BooleanArgOptionDefinition(
        shortopt: Option[String],
        longopt: String,
        valueName: String,
        description: String,
        action: Boolean => Unit
        ) extends OptionDefinition(true, shortopt, longopt, null, valueName,
          description, { a: String =>
    val boolValue = a.toLowerCase match {
      case "true" => true
      case "false" => false
      case "yes" => true
      case "no" => false
      case "1" => true
      case "0" => false
      case _ =>
        throw new IllegalArgumentException("Expected a string I can interpret as a boolean")
    }
    action(boolValue)},
    true, false)

class KeyValueArgOptionDefinition(
        shortopt: Option[String],
        longopt: String,
        keyName: String,
        valueName: String,
        description: String,
        action: (String, String) => Unit
        ) extends OptionDefinition(true, shortopt, longopt, keyName, valueName,
          description, { a: String =>
  a.indexOf('=') match {
    case -1     => throw new IllegalArgumentException("Expected a key=value pair")
    case n: Int => action(a.dropRight(a.length - n), a.drop(n + 1))
  }},
  false, true)

class KeyIntValueArgOptionDefinition(
        shortopt: Option[String],
        longopt: String,
        keyName: String,
        valueName: String,
        description: String,
        action: (String, Int) => Unit
        ) extends OptionDefinition(true, shortopt, longopt, keyName, valueName,
          description, { a: String =>
  a.indexOf('=') match {
    case -1     => throw new IllegalArgumentException("Expected a key=value pair")
    case n: Int => action(a.dropRight(a.length - n), a.drop(n + 1).toInt)
  }},
  false, true)

class KeyDoubleValueArgOptionDefinition(
        shortopt: Option[String],
        longopt: String,
        keyName: String,
        valueName: String,
        description: String,
        action: (String, Double) => Unit
        ) extends OptionDefinition(true, shortopt, longopt, keyName, valueName,
          description, { a: String =>
  a.indexOf('=') match {
    case -1     => throw new IllegalArgumentException("Expected a key=value pair")
    case n: Int => action(a.dropRight(a.length - n), a.drop(n + 1).toDouble)
  }},
  false, true)

class KeyBooleanValueArgOptionDefinition(
        shortopt: Option[String],
        longopt: String,
        keyName: String,
        valueName: String,
        description: String,
        action: (String, Boolean) => Unit
        ) extends OptionDefinition(true, shortopt, longopt, null, valueName,
          description, { a: String =>
    if (!a.contains("="))
      throw new IllegalArgumentException("Expected a key=value pair")

    val key = a.dropRight(a.length - a.indexOf('='))
    val boolValue = a.drop(a.indexOf('=') + 1).toLowerCase match {
      case "true" => true
      case "false" => false
      case "yes" => true
      case "no" => false
      case "1" => true
      case "0" => false
      case _ =>
        throw new IllegalArgumentException("Expected a string I can interpret as a boolean")
    }
    action(key, boolValue)},
    false, true)

class FlagOptionDefinition(
        shortopt: Option[String],
        longopt: String,
        description: String,
        action: => Unit
        ) extends OptionDefinition(true, shortopt, longopt, null, null,
          description, {a: String => action}, false, false)

/**
 * OptionParser is instantiated within your object,
 * set up by an (ordered) sequence of invocations of
 * the various builder methods such as #opt or #arg
 */
case class OptionParser(
        programName: Option[String],
        version: Option[String],
        errorOnUnknownArgument: Boolean) {
  def this() = this(None, None, true)
  def this(programName: String) = this(Some(programName), None, true)
  def this(programName: String, version: String) = this(Some(programName), Some(version), true)
  def this(errorOnUnknownArgument: Boolean) = this(None, None, errorOnUnknownArgument)
  def this(programName: String, errorOnUnknownArgument: Boolean) =
    this(Some(programName), None , errorOnUnknownArgument)

  val options = new ListBuffer[OptionDefinition]
  val arguments = new ListBuffer[Argument]
  val NL = System.getProperty("line.separator")
  val TB = "        "
  val NLTB = NL + TB
  val NLNL = NL + NL
  val defaultKeyName = "<key>"
  val defaultValueName = "<value>"
  var argList: Option[Argument] = None

  // -------- Defining options ---------------
  def add(option: OptionDefinition) {
    option match {
      case a: Argument =>
        if (a.allowMultiple)
          argList = Some(a)
        else
          arguments += a
      case _ => options += option
    }
  }

  // setup options (which require -char or --string to invoke
  def opt(shortopt: String, longopt: String, description: String, action: String => Unit) =
    add(new ArgOptionDefinition(Some(shortopt), longopt, defaultValueName, description, action))

  def opt(longopt: String, description: String, action: String => Unit) =
    add(new ArgOptionDefinition(None, longopt, defaultValueName, description, action))

  def opt(shortopt: String, longopt: String, valueName: String,
      description: String, action: String => Unit) =
    add(new ArgOptionDefinition(Some(shortopt), longopt, valueName, description, action))

  def opt(shortopt: Option[String], longopt: String, valueName: String,
      description: String, action: String => Unit) =
    add(new ArgOptionDefinition(shortopt, longopt, valueName, description, action))

  def opt(shortopt: String, longopt: String, description: String, action: => Unit) =
    add(new FlagOptionDefinition(Some(shortopt), longopt, description, action))

  def opt(longopt: String, description: String, action: => Unit) =
    add(new FlagOptionDefinition(None, longopt, description, action))

  // we have to give these typed options separate names, because of &^@$! type erasure
  def intOpt(shortopt: String, longopt: String, description: String, action: Int => Unit) =
    add(new IntArgOptionDefinition(Some(shortopt), longopt, defaultValueName, description, action))

  def intOpt(longopt: String, description: String, action: Int => Unit) =
    add(new IntArgOptionDefinition(None, longopt, defaultValueName, description, action))

  def intOpt(shortopt: String, longopt: String, valueName: String,
      description: String, action: Int => Unit) =
    add(new IntArgOptionDefinition(Some(shortopt), longopt, valueName, description, action))

  def intOpt(shortopt: Option[String], longopt: String, valueName: String,
      description: String, action: Int => Unit) =
    add(new IntArgOptionDefinition(shortopt, longopt, valueName, description, action))

  def doubleOpt(shortopt: String, longopt: String, description: String, action: Double => Unit) =
    add(new DoubleArgOptionDefinition(Some(shortopt), longopt, defaultValueName, description, action))

  def doubleOpt(longopt: String, description: String, action: Double => Unit) =
    add(new DoubleArgOptionDefinition(None, longopt, defaultValueName, description, action))

  def doubleOpt(shortopt: String, longopt: String, valueName: String,
      description: String, action: Double => Unit) =
    add(new DoubleArgOptionDefinition(Some(shortopt), longopt, valueName, description, action))

  def doubleOpt(shortopt: Option[String], longopt: String, valueName: String,
      description: String, action: Double => Unit) =
    add(new DoubleArgOptionDefinition(shortopt, longopt, valueName, description, action))

  def booleanOpt(shortopt: String, longopt: String, description: String, action: Boolean => Unit) =
    add(new BooleanArgOptionDefinition(Some(shortopt), longopt, defaultValueName, description, action))

  def booleanOpt(longopt: String, description: String, action: Boolean => Unit) =
    add(new BooleanArgOptionDefinition(None, longopt, defaultValueName, description, action))

  def booleanOpt(shortopt: String, longopt: String, valueName: String,
      description: String, action: Boolean => Unit) =
    add(new BooleanArgOptionDefinition(Some(shortopt), longopt, valueName, description, action))

  def booleanOpt(shortopt: Option[String], longopt: String, valueName: String,
      description: String, action: Boolean => Unit) =
    add(new BooleanArgOptionDefinition(shortopt, longopt, valueName, description, action))

  def keyValueOpt(shortopt: String, longopt: String, description: String, action: (String, String) => Unit) =
    add(new KeyValueArgOptionDefinition(Some(shortopt), longopt, defaultKeyName, defaultValueName, description, action))

  def keyValueOpt(longopt: String, description: String, action: (String, String) => Unit) =
    add(new KeyValueArgOptionDefinition(None, longopt, defaultKeyName, defaultValueName, description, action))

  def keyValueOpt(shortopt: String, longopt: String, keyName: String, valueName: String,
      description: String, action: (String, String) => Unit) =
    add(new KeyValueArgOptionDefinition(Some(shortopt), longopt, keyName, valueName, description, action))

  def keyValueOpt(shortopt: Option[String], longopt: String, keyName: String, valueName: String,
      description: String, action: (String, String) => Unit) =
    add(new KeyValueArgOptionDefinition(shortopt, longopt, keyName, valueName, description, action))

  def keyIntValueOpt(shortopt: String, longopt: String, description: String, action: (String, Int) => Unit) =
    add(new KeyIntValueArgOptionDefinition(Some(shortopt), longopt, defaultKeyName, defaultValueName, description, action))

  def keyIntValueOpt(longopt: String, description: String, action: (String, Int) => Unit) =
    add(new KeyIntValueArgOptionDefinition(None, longopt, defaultKeyName, defaultValueName, description, action))

  def keyIntValueOpt(shortopt: String, longopt: String, keyName: String, valueName: String,
      description: String, action: (String, Int) => Unit) =
    add(new KeyIntValueArgOptionDefinition(Some(shortopt), longopt, keyName, valueName, description, action))

  def keyIntValueOpt(shortopt: Option[String], longopt: String, keyName: String, valueName: String,
      description: String, action: (String, Int) => Unit) =
    add(new KeyIntValueArgOptionDefinition(shortopt, longopt, keyName, valueName, description, action))

  def keyDoubleValueOpt(shortopt: String, longopt: String, description: String, action: (String, Double) => Unit) =
    add(new KeyDoubleValueArgOptionDefinition(Some(shortopt), longopt, defaultKeyName, defaultValueName, description, action))

  def keyDoubleValueOpt(longopt: String, description: String, action: (String, Double) => Unit) =
    add(new KeyDoubleValueArgOptionDefinition(None, longopt, defaultKeyName, defaultValueName, description, action))

  def keyDoubleValueOpt(shortopt: String, longopt: String, keyName: String, valueName: String,
      description: String, action: (String, Double) => Unit) =
    add(new KeyDoubleValueArgOptionDefinition(Some(shortopt), longopt, keyName, valueName, description, action))

  def keyDoubleValueOpt(shortopt: Option[String], longopt: String, keyName: String, valueName: String,
      description: String, action: (String, Double) => Unit) =
    add(new KeyDoubleValueArgOptionDefinition(shortopt, longopt, keyName, valueName, description, action))

  def keyBooleanValueOpt(shortopt: String, longopt: String, description: String, action: (String, Boolean) => Unit) =
    add(new KeyBooleanValueArgOptionDefinition(Some(shortopt), longopt, defaultKeyName, defaultValueName, description, action))

  def keyBooleanValueOpt(longopt: String, description: String, action: (String, Boolean) => Unit) =
    add(new KeyBooleanValueArgOptionDefinition(None, longopt, defaultKeyName, defaultValueName, description, action))

  def keyBooleanValueOpt(shortopt: String, longopt: String, keyName: String, valueName: String,
      description: String, action: (String, Boolean) => Unit) =
    add(new KeyBooleanValueArgOptionDefinition(Some(shortopt), longopt, keyName, valueName, description, action))

  def keyBooleanValueOpt(shortopt: Option[String], longopt: String, keyName: String, valueName: String,
      description: String, action: (String, Boolean) => Unit) =
    add(new KeyBooleanValueArgOptionDefinition(shortopt, longopt, keyName, valueName, description, action))

  def help(shortopt: String, longopt: String, description: String = "show this help message") =
    add(new FlagOptionDefinition(Some(shortopt), longopt, description, {this.showUsage; exit}))

  def help(shortopt: Option[String], longopt: String, description: String) =
    add(new FlagOptionDefinition(shortopt, longopt, description, {this.showUsage; exit}))

  def separator(description: String) =
    add(new SeparatorDefinition(description))

  // regular arguments without the - or -- which have a name purely for help
  def arg(name: String, description: String, action: String => Unit) =
    add(new Argument(name, description, false, action))

  // arglist allows multiple arguments
  def arglist(name: String, description: String, action: String => Unit) =
    add(new Argument(name, description, true, action))

  // -------- Getting usage information ---------------
  def descriptions: Seq[String] = options.map(opt => opt match {
    //case x: Argument => x.longopt + " :" + NLTB + opt.description
    case x if !x.canBeInvoked => x.description
    case x if x.keyValueArgument =>
      (x.shortopt map { o => "-" + o + ":" + x.keyName + "=" + x.valueName + " | " } getOrElse { "" }) +
      "--" + x.longopt + ":" + x.keyName + "=" + x.valueName + NLTB + x.description
    case x if x.gobbleNextArgument =>
      (x.shortopt map { o => "-" + o + " " + x.valueName + " | " } getOrElse { "" }) +
      "--" + x.longopt + " " + x.valueName + NLTB + x.description
    case _ =>
      (opt.shortopt map { o => "-" + o + " | " } getOrElse { "" }) +
      "--" + opt.longopt + NLTB + opt.description
  }) ++= (argList match {
    case Some(x: Argument) => List(x.valueName + NLTB + x.description)
    case None              => arguments.map(a => a.valueName + NLTB + a.description)
  })

  def usage: String = {
    val prorgamText = programName map { _ + " " } getOrElse { "" }
    val versionText = programName map { pg =>
      version map { NL + pg + " " + _ } getOrElse { "" }
    } getOrElse { "" }
    val optionText = if (options.isEmpty) {""} else {"[options] "}
    val argumentList = argumentNames.mkString(" ")

    versionText + NL + "Usage: " + prorgamText + optionText + argumentList + NLNL +
    "  " + descriptions.mkString(NL + "  ") + NL
  }

  def showUsage = Console.err.println(usage)

  def argumentNames: Seq[String] = argList match {
    case Some(x: Argument) => List(x.valueName)
    case None              => arguments.map(_.valueName)
  }

  def applyArgument(option:OptionDefinition, arg:String) :Boolean ={
      try {
        option.action.apply(arg)
        true
      } catch {
        case e:NumberFormatException => System.err.println("Error: " +
                option.shortDescription + " expects a number but was given '" + arg + "'")
        false
        case e:Throwable => System.err.println("Error: " +
                option.shortDescription + " failed when given '" + arg + "'. " + e.getMessage)
        false
      }
  }

  // -------- Parsing ---------------
  def parse(args: Seq[String]): Boolean = {
    var i = 0
    val requiredArgs = arguments.clone
    var answer = true
    var argListCount = 0
    var indexOutOfBounds = false

    while (i < args.length) {
      val arg = args(i)
      val matchingOption = options.find(opt =>
        opt.canBeInvoked &&
          ((!opt.keyValueArgument &&
            (arg == "--" + opt.longopt ||
            (opt.shortopt map { o => arg == "-" + o } getOrElse { false }))) ||
          (opt.keyValueArgument &&
            (arg.startsWith("--" + opt.longopt + ":") ||
            (opt.shortopt map { o => arg.startsWith("-" + o + ":") } getOrElse { false }))))
      )

      matchingOption match {
        case None =>
          if (arg.startsWith("-")) {
            if (errorOnUnknownArgument) {
              System.err.println("Error: Unknown argument '" + arg + "'")
              answer = false
            } else
              System.err.println("Warning: Unknown argument '" + arg + "'")
          } else if (argList.isDefined) {
            argListCount += 1
            if (!applyArgument(argList.get, arg)) {
              answer = false
            }
          } else if (requiredArgs.isEmpty) {
            if (errorOnUnknownArgument) {
              System.err.println("Error: Unknown argument '" + arg + "'")
              answer = false
            } else
              System.err.println("Warning: Unknown argument '" + arg + "'")
          } else {
            val first = requiredArgs.remove(0)
            if (!applyArgument(first, arg)) {
              answer = false
            }
          }

        case Some(option) =>
          val argToPass = if (option.gobbleNextArgument) {
            i += 1;

            if (i >= args.length) {
              indexOutOfBounds = true
              if (errorOnUnknownArgument) {
                System.err.println("Error: missing value after '" + arg + "'")
                answer = false
              } else
                System.err.println("Warning: missing value after '" + arg + "'")
              ""
            } else
              args(i)
          } else if (option.keyValueArgument &&
              (option.shortopt map { o => arg.startsWith("-" + o + ":") } getOrElse { false })) {
            arg.drop(("-" + option.shortopt.get + ":").length)
          } else if (option.keyValueArgument &&
              arg.startsWith("--" + option.longopt + ":")) {
            arg.drop(("--" + option.longopt + ":").length)
          } else
            ""

          if (!indexOutOfBounds && !applyArgument(option, argToPass)) {
            answer = false
          }
      }
      i += 1
    }

    if (!requiredArgs.isEmpty ||
        (argListCount == 0 && argList.isDefined)) {
      System.err.println("Error: missing arguments: " + argumentNames.mkString(", "))
      answer = false
    }
    if (!answer)
      showUsage
    answer
  }
}
