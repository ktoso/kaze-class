package pl.project13.kaze

import scala.annotation.tailrec
import scala.collection.immutable

object KazeClass {

  import scala.reflect.ClassTag

  def of[T](implicit tag: ClassTag[T]) =
    new MkKazeClazz(tag.runtimeClass)

  case class MkKazeClazz(clazz: Class[_], useAkkaUtils: Boolean = true, genEquals: Boolean = false, genConfig: Boolean = false, genJavaApi: Boolean = true) {

    def withAkkaUtils: MkKazeClazz = this.copy(useAkkaUtils = true)
    def withoutAkkaUtils: MkKazeClazz = this.copy(useAkkaUtils = false)

    def withEqualsHashcode: MkKazeClazz = this.copy(genEquals = true)

    def withConfig: MkKazeClazz = this.copy(genConfig = true)

    def withoutJavaApi: MkKazeClazz = this.copy(genJavaApi = false)

    import java.lang.reflect.Field

    private var indent = ""

    private val clazzName = simpleName(clazz)

    private val skipThose = Set(
      "apply", "unapply", "curried", "tupled",
      "canEqual", "equals", "toString", "hashCode", "copy",
      "productIterator", "productPrefix", "productArity", "productElement"
    )

    // full class name not needed for these packages
    private val knownPackages = List("java.lang.", "scala.", clazz.getPackage.getName + ".")
    // full class name not needed for these classes
    private val knownClasses = List(classOf[List[_]].getName, classOf[Set[_]].getName, classOf[IndexedSeq[_]].getName,
      classOf[Map[_, _]].getName)
    private val immutableSeq = "scala.collection.immutable.Seq["
    private val immutableSeqShort = "immutable.Seq["

    private val scalaDuration = "scala.concurrent.duration.FiniteDuration"

    private val fields = clazz.getDeclaredFields
      .filterNot(skip)
      .filterNot(_.getName.contains("$"))

    def toClipboard(): Unit = {
      import java.awt.Toolkit
      import java.awt.datatransfer.StringSelection

      val selection = new StringSelection(render)
      Toolkit.getDefaultToolkit.getSystemClipboard.setContents(selection, selection)
    }

    def render: String = {
      val sb = new StringBuilder(
        s"""import scala.collection.immutable
           |""".stripMargin)
      if (genJavaApi) {
        sb.append(
          """import scala.collection.JavaConverters._
            |import scala.compat.java8.OptionConverters._
            |""".stripMargin)
        if (useAkkaUtils)
          sb.append(
            """import akka.util.JavaDurationConverters._
              |""".stripMargin)
        else
          sb.append(
            """import scala.compat.java8.DurationConverters
              |""".stripMargin)
      }
      sb.append(
        s"""
           |${indent}final class $clazzName private(\n""".stripMargin)
      indent = "  "

      renderConstructor(sb)

      sb.append("{\n\n")

      if (genJavaApi) {
        renderJavaAccessors(sb)

        sb.append("\n")
      }

      renderWithXyz(sb)
      sb.append("\n")
      renderCopy(sb)
      sb.append("\n")
      renderToString(sb)
      if (genEquals) {
        sb.append("\n")
        renderEquals(sb)
        sb.append("\n")
        renderHashCode(sb)
      }

      sb.append("}\n\n") // end class

      // companion object
      indent = "  "
      sb.append(s"object $clazzName {\n")
      sb.append(s"${indent}/** Scala API */\n")
      sb.append(s"${indent}def apply(): $clazzName = new $clazzName()\n")

      if (genJavaApi) {
        sb.append(s"${indent}/** Java API */\n")
        sb.append(s"${indent}def create(): $clazzName = apply()\n")
      }

      if (genConfig) {
        renderFromConfig(sb)
        renderSampleConfig(sb)
      }

      renderObjectApply(sb)
      if (genJavaApi) {
        renderObjectCreate(sb)
      }
      sb.append("}\n")

      sb.result()
    }

    private def renderConstructor(sb: StringBuilder): Unit = {
      val skipExtends = Set("java.lang.Object", "scala.Product", "scala.Serializable")
      fields.foreach { m =>
        val mName = m.getName
        val mType = theType(m)
        sb.append(s"${indent}val $mName: $mType,\n")
      }
      sb.delete(sb.length - 2, sb.length)
      sb.append("\n) ")
      val extending = (Seq(clazz.getSuperclass.getName) ++ clazz.getInterfaces.map(_.getName))
        .filterNot(skipExtends.contains)
      if (extending.nonEmpty) sb.append(extending.mkString("extends ", "\nwith ", " "))
    }

    private def renderJavaAccessors(sb: StringBuilder): Unit = {
      fields.foreach { m =>
        sb.append(s"${indent}/** Java API */\n")
        val mName = m.getName
        val mType = theType(m)
        if (mType.startsWith("Option[")
          || mType.startsWith("List[")
          || mType.startsWith(immutableSeqShort)) {
          sb.append(s"${indent}def get${up(mName)}: ${theJavaType(mType)} = $mName.asJava\n")
        } else if (mType == scalaDuration) {
          sb.append(s"${indent}def get${up(mName)}: ${theJavaType(mType)} = $mName.${if (useAkkaUtils) "as" else "to"}Java\n")
        } else if (mType == "Boolean") {
          sb.append(s"${indent}def is${up(mName)}: Boolean = $mName\n")
        } else {
          sb.append(s"${indent}def get${up(mName)}: $mType = $mName\n")
        }
      }
    }

    private def renderWithXyz(sb: StringBuilder): Unit = {
      fields.foreach { m =>
        val mName = m.getName
        val mType = theType(m)
        val jType = theJavaType(mType)
        if (mType.startsWith("Option[")) {
          val mTypeWithoutOpt = mType.substring("Option[".length, mType.length - 1)
          sb.append(s"${indent}def with${up(mName)}(value: $mTypeWithoutOpt): $clazzName = copy($mName = Option(value))\n")
        } else if (mType.startsWith("List[")) {
          appendScalaApi(sb, indent)
          sb.append(s"${indent}def with${up(mName)}(values: $mType): $clazzName = copy($mName = values)\n")
          if (genJavaApi) {
            appendJavaApi(sb, indent)
            sb.append(s"${indent}def with${up(mName)}(values: $jType): $clazzName = copy($mName = values.asScala.toList)\n")
          }
        } else if (mType.startsWith(immutableSeqShort)) {
          appendScalaApi(sb, indent)
          sb.append(s"${indent}def with${up(mName)}(values: $mType): $clazzName = copy($mName = values)\n")
          if (genJavaApi) {
            appendJavaApi(sb, indent)
            sb.append(s"${indent}def with${up(mName)}(values: $jType): $clazzName = copy($mName = values.asScala.toList)\n")
          }
        } else if (mType == scalaDuration) {
          appendScalaApi(sb, indent)
          sb.append(s"${indent}def with${up(mName)}(value: $mType): $clazzName = copy($mName = value)\n")
          if (genJavaApi) {
            appendJavaApi(sb, indent)
            sb.append(s"${indent}def with${up(mName)}(value: $jType): $clazzName = copy($mName = value.${if (useAkkaUtils) "as" else "to"}Scala)\n")
          }
        } else if (mType == "Boolean") {
          sb.append(s"${indent}def with${up(mName)}(value: Boolean): $clazzName = if ($mName == value) this else copy($mName = value)\n")
        } else {
          sb.append(s"${indent}def with${up(mName)}(value: $mType): $clazzName = copy($mName = value)\n")
        }
      }
    }

    private def renderCopy(sb: StringBuilder): Unit = {
      sb.append(s"${indent}private def copy(\n")
      indent += "  "
      for {
        m <- fields
        mName = m.getName
        mType = theType(m)
      } sb.append(s"$indent$mName: $mType = $mName,\n")
      sb.delete(sb.length - 2, sb.length)
      indent = indent.dropRight(2)
      sb.append(s"\n$indent): $clazzName = ").append(s"new $clazzName(\n")
      indent += "    "
      for {
        m <- fields
        mName = m.getName
      } sb.append(s"$indent$mName = $mName,\n")
      sb.delete(sb.length - 2, sb.length)
      indent = indent.dropRight(2)
      sb.append(s"\n$indent)\n")
    }

    private def renderToString(sb: StringBuilder): Unit = {
      indent = "  "
      sb.append(s"${indent}override def toString =\n")
      indent += " " * 2
      sb.append(s"""${indent}s\"\"\"${clazzName}(""")
      for {
        m <- fields
        mName = m.getName
      } sb.append(s"$mName=$$$mName,")
      sb.delete(sb.length - 1, sb.length)
      sb.append(s""")\"\"\"""")
      sb.append("\n")
    }

    private def renderEquals(sb: StringBuilder): Unit = {
      indent = "  "
      sb.append(s"${indent}override def equals(other: Any): Boolean = other match {\n")
      sb.append(s"${indent}  case that: ${clazzName} =>\n")
      indent += "    "
      for {
        m <- fields
        mName = m.getName
      } sb.append(s"${indent}java.util.Objects.equals(this.$mName, that.$mName) &&\n")
      sb.delete(sb.length - 4, sb.length)
      sb.append("\n")
      sb.append(s"${indent}  case _ => false\n")
      sb.append("  }\n")
    }

    private def renderHashCode(sb: StringBuilder): Unit = {
      val box = Set("int", "long", "boolean")
      val wrapper = Map("int" -> "Int.box", "long" -> "Long.box", "boolean" -> "Boolean.box")

      indent = "  "
      sb.append(s"${indent}override def hashCode(): Int =\n")
      sb.append(s"${indent}  java.util.Objects.hash(\n")
      indent += "    "
      for {
        m <- fields
        mName = m.getName
      }
        if (box.contains(m.getType.getName)) {
          sb.append(s"${indent}${wrapper(m.getType.getName)}($mName),\n")
        } else sb.append(s"${indent}$mName,\n")
      sb.delete(sb.length - 2, sb.length)
      sb.append(")\n")
    }

    private def renderFromConfig(sb: StringBuilder): Unit = {
      sb.append(s"${indent}/**\n")
      sb.append(s"${indent} * Reads from the given config.\n")
      sb.append(s"${indent} */\n")
      sb.append(s"${indent}def apply(c: Config): $clazzName = {\n")
      indent = indent + "  "
      fields.foreach { m =>
        val mName = m.getName
        val mType = theType(m)
        val jType = theJavaType(mType)
        val cName = mName // TODO hyphenize?
        if (mType == "Option[Int]") {
          sb.append(s"""${indent}val $mName = if (c.hasPath("$cName")) Some(c.getInt("$cName")) else None\n""")
        } else if (mType == "Option[String]") {
          sb.append(s"""${indent}val $mName = if (c.hasPath("$cName")) Some(c.getString("${cName}")) else None\n""")
        } else if (mType == s"Option[$scalaDuration]") {
          sb.append(s"""${indent}val $mName = if (c.hasPath("$cName")) Some(c.getDuration("${cName}").${if (useAkkaUtils) "as" else "to"}Scala) else None\n""")
        } else if (mType == "Int") {
          sb.append(s"""${indent}val $mName = c.getInt("$cName")\n""")
        } else if (mType == "String") {
          sb.append(s"""${indent}val $mName = c.getString("$cName")\n""")
        } else if (mType == scalaDuration) {
          sb.append(s"""${indent}val $mName = c.getDuration("$cName").asScala\n""")
        } else if (mType == "Boolean") {
          sb.append(s"""${indent}val $mName = c.getBoolean("$cName")\n""")
        } else {
          sb.append(s"""${indent}val $mName = c.get ("$cName")\n""")
        }
      }
      sb.append(s"${indent}apply(\n")
      fields.foreach { m =>
        val mName = m.getName
        val mType = theType(m)
        sb.append(s"${indent}  $mName,\n")
      }
      sb.delete(sb.length - 2, sb.length)
      sb.append(s"\n${indent})\n")
      indent = indent.dropRight(2)
      sb.append(s"${indent}}\n")
      sb.append("\n")
    }

    private def renderSampleConfig(sb: StringBuilder): Unit = {
      sb.append(s"${indent}/* sample config section\n")
      sb.append(s"${indent}{\n")
      indent = indent + "  "
      fields.foreach { m =>
        val mName = m.getName
        val mType = theType(m)
        val jType = theJavaType(mType)
        val cName = mName
        if (mType == "Option[Int]") {
          sb.append(s"""${indent}$cName = 1234567 # optional\n""")
        } else if (mType == "Option[String]") {
          sb.append(s"""${indent}$cName = "some text" # optional\n""")
        } else if (mType == s"Option[$scalaDuration]") {
          sb.append(s"""${indent}$cName = 50 seconds # optional\n""")
        } else if (mType == "Int") {
          sb.append(s"""${indent}$cName = 1234567\n""")
        } else if (mType == "String") {
          sb.append(s"""${indent}$cName = "some text"\n""")
        } else if (mType == scalaDuration) {
          sb.append(s"""${indent}$cName = 50 seconds\n""")
        } else if (mType == "Boolean") {
          sb.append(s"""${indent}$cName = false\n""")
        } else {
          sb.append(s"""${indent}$cName = ???\n""")
        }
      }
      indent = indent.dropRight(2)
      sb.append(s"${indent}}\n")
      sb.append(s"${indent}*/\n")
      sb.append("\n")
    }

    private def renderObjectApply(sb: StringBuilder): Unit = {
      // Scala: all fields apply method
      appendScalaApi(sb, indent)
      sb.append(s"${indent}def apply(\n")
      fields.foreach { m =>
        val mName = m.getName
        val mType = theType(m)
        sb.append(s"${indent}  $mName: $mType,\n")
      }
      sb.delete(sb.length - 2, sb.length)
      sb.append("\n")
      sb.append(s"$indent): $clazzName = new $clazzName(\n")
      fields.foreach { m =>
        val mName = m.getName
        val mType = theType(m)
        sb.append(s"${indent}  $mName,\n")
      }
      sb.delete(sb.length - 2, sb.length)
      sb.append("\n")
      sb.append(s"${indent})\n")
      sb.append("\n")
    }

    private def renderObjectCreate(sb: StringBuilder): Unit = {
      // Java: all fields create method
      appendJavaApi(sb, indent)
      sb.append(s"${indent}def create(\n")
      fields.foreach { m =>
        val mName = m.getName
        val mType = theType(m)
        val jType = theJavaType(mType)

        sb.append(s"${indent}  $mName: $jType,\n")
      }
      sb.delete(sb.length - 2, sb.length)
      sb.append("\n")
      sb.append(s"$indent): $clazzName = new $clazzName(\n")
      fields.foreach { m =>
        val mName = m.getName
        val mType = theType(m)
        if (mType.startsWith("List[") || mType.startsWith(immutableSeqShort)) {
          sb.append(s"${indent}  $mName.asScala.toList,\n")
        } else if (mType.startsWith("Option[") || mType == scalaDuration) {
          sb.append(s"${indent}  $mName.asScala,\n")
        } else
          sb.append(s"${indent}  $mName,\n")
      }
      sb.delete(sb.length - 2, sb.length)
      sb.append("\n")
      sb.append(s"${indent})\n")
    }

    private def appendScalaApi(sb: StringBuilder, indent: String): Unit =
      sb.append(s"${indent}/** Scala API */\n")

    private def appendJavaApi(sb: StringBuilder, indent: String): Unit =
      sb.append(s"${indent}/** Java API */\n")

    private def theType(m: Field): String = {
      val raw = m.getGenericType.toString.replaceAll("<", "[").replaceAll(">", "]")
      raw match {
        case "boolean" | "int" | "long" | "byte" | "short" | "double" | "float" => up(raw)
        case name if name.startsWith("class ") => removeKnownPackages(m.getName(), name.replaceAll("class ", ""))
        case name if name.startsWith("interface ") => removeKnownPackages(m.getName(), name.substring("interface ".length))
        case _ => removeKnownPackages(m.getName(), raw)
      }
    }

    private def theJavaType(mType: String): String =
      if (mType.startsWith("Option[")) {
        mType.replace("Option", "java.util.Optional")
      } else if (mType.startsWith("List[")) {
        mType.replace("List", "java.util.List")
      } else if (mType.startsWith(immutableSeqShort)) {
        mType.replace(immutableSeqShort, "java.util.List[")
      } else if (mType == scalaDuration)
        "java.time.Duration"
      else mType

    private def removeKnownPackages(fieldName: String, clsName: String): String = {

      @tailrec def removePkg(name: String, remaining: List[String]): String = remaining match {
        case Nil => name
        case pkg :: tail =>
          if (name.startsWith(pkg) && name.charAt(pkg.length).isUpper) name.drop(pkg.length)
          else removePkg(name, tail)
      }

      def removePackageFromClass(name: String): String =
        if (name.startsWith(immutableSeq)) "immutable.Seq[" + name.drop(immutableSeq.length)
        else removeClass(name, knownClasses)

      @tailrec def removeClass(name: String, remaining: List[String]): String = remaining match {
        case Nil => name
        case `name` :: _ => simpleName(name)
        case n :: _ if name.startsWith(n + "[") => simpleName(name.take(n.length)) + name.drop(n.length)
        case _ :: tail => removeClass(name, tail)
      }

      val clsName2 =
        if (clsName.endsWith("]")) {
          val i = clsName.lastIndexOf('[')
          val genericTypeName = removeKnownPackages(fieldName, clsName.substring(i + 1, clsName.length - 1))
          clsName.take(i) + "[" + genericTypeName + "]"
        } else clsName

      if (clsName2 == "java.lang.Object") {
        s"${fieldName}ErasedType"
      } else {
        val clsName3 = removePkg(clsName2, knownPackages)
        removePackageFromClass(clsName3)
      }
    }

    private def skip(m: Field) = skipThose(m.getName)

    private def simpleName(clazz: Class[_]): String = {
      simpleName(clazz.getName)
    }

    private def simpleName(className: String): String = {
      val i = className.lastIndexOf('.')
      className.substring(i + 1)
    }

    private def up(s: String): String = s.charAt(0).toUpper + s.tail
  }

}
