package pl.project13.kaze

import scala.annotation.tailrec
import scala.collection.immutable

object KazeClass {

  import scala.reflect.ClassTag

  def of[T](implicit tag: ClassTag[T]) =
    new MkKazeClazz(tag.runtimeClass)

  class MkKazeClazz(clazz: Class[_]) {

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

    def render = {
      val sb = new StringBuilder(s"${indent}final class $clazzName private(\n")
      indent = "  "

      renderConstructor(sb)

      sb.append("{\n\n")

      renderJavaAccessors(sb)

      sb.append("\n")

      renderWithXyz(sb)
      sb.append("\n")
      renderCopy(sb)
      sb.append("\n")
      renderToString(sb)
      sb.append("\n")
      renderEquals(sb)
      sb.append("\n")
      renderHashCode(sb)

      sb.append("}\n") // end class

      // companion object
      indent = "  "
      sb.append(s"object $clazzName {\n")
      sb.append(s"${indent}/** Scala API */\n")
      sb.append(s"${indent}def apply(): $clazzName = new $clazzName()\n")

      sb.append(s"${indent}/** Java API */\n")
      sb.append(s"${indent}def create(): $clazzName = apply()\n")

      renderObjectApply(sb)
      renderObjectCreate(sb)

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
      sb.append(")")
      val extending = (Seq(clazz.getSuperclass.getName) ++ clazz.getInterfaces.map(_.getName))
        .filterNot(skipExtends.contains)
      if (extending.nonEmpty) sb.append(extending.mkString("\nextends ", "\nwith ", " "))
    }

    private def renderJavaAccessors(sb: StringBuilder): Unit = {
      fields.foreach { m =>
        sb.append(s"${indent}/** Java API */\n")
        val mName = m.getName
        val mType = theType(m)
        if (mType.startsWith("Option[")
          || mType.startsWith("List[")
          || mType.startsWith(immutableSeqShort)
          || mType == scalaDuration) {
          sb.append(s"${indent}def get${up(mName)}: ${theJavaType(mType)} = $mName.asJava\n")
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
          appendJavaApi(sb, indent)
          sb.append(s"${indent}def with${up(mName)}(values: $jType): $clazzName = copy($mName = values.asScala.toList)\n")
        } else if (mType.startsWith(immutableSeqShort)) {
          appendScalaApi(sb, indent)
          sb.append(s"${indent}def with${up(mName)}(values: $mType): $clazzName = copy($mName = values)\n")
          appendJavaApi(sb, indent)
          sb.append(s"${indent}def with${up(mName)}(values: $jType): $clazzName = copy($mName = values.asScala.toList)\n")
        } else if (mType == scalaDuration) {
          appendScalaApi(sb, indent)
          sb.append(s"${indent}def with${up(mName)}(value: $mType): $clazzName = copy($mName = value)\n")
          appendJavaApi(sb, indent)
          sb.append(s"${indent}def with${up(mName)}(value: $jType): $clazzName =\n" +
            s"${indent}  with${up(mName)}($scalaDuration(value.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS))\n")
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
      sb.append(s"): $clazzName = ").append(s"new $clazzName(\n")

      indent += "  "
      for {
        m <- fields
        mName = m.getName
      } sb.append(s"$indent$mName = $mName,\n")
      sb.delete(sb.length - 2, sb.length)
      sb.append(")\n")
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
        case name if name.startsWith("class ") => removeKnownPackages(name.replaceAll("class ", ""))
        case _ => removeKnownPackages(raw)
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

    private def removeKnownPackages(clsName: String): String = {

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
          val genericTypeName = removeKnownPackages(clsName.substring(i + 1, clsName.length - 1))
          clsName.take(i) + "[" + genericTypeName + "]"
        } else clsName

      val clsName3 = removePkg(clsName2, knownPackages)
      removePackageFromClass(clsName3)
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
