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
      classOf[immutable.Seq[_]].getName, classOf[Map[_, _]].getName)

    private val fields = clazz.getDeclaredFields
        .filterNot(skip)
        .filterNot(_.getName.contains("$"))

    def render = {
      val sb = new StringBuilder(s"${indent}final class $clazzName private(\n")
      indent = "  "

      // constructor

      fields.foreach { m =>
        val mName = m.getName
        val mType = theType(m)
        sb.append(s"${indent}val $mName: $mType,\n")
      }
      sb.delete(sb.length - 2 , sb.length)
      sb.append(") {\n")
      sb.append("\n")

      // with...

      fields.foreach { m =>
        val mName = m.getName
        val mType = theType(m)
        if (mType.startsWith("Option[")) {
          val mTypeWithoutOpt = mType.substring("Option[".length, mType.length - 1)
          sb.append(s"${indent}def with${up(mName)}(value: $mTypeWithoutOpt): $clazzName = copy($mName = Option(value))\n")
        } else
          sb.append(s"${indent}def with${up(mName)}(value: $mType): $clazzName = copy($mName = value)\n")
      }

      sb.append("\n")

      // copy method

      sb.append(s"${indent}private def copy(\n")
      indent += "  "
      for {
        m <- fields
        mName = m.getName
        mType = theType(m)
      } sb.append(s"$indent$mName: $mType = $mName,\n")
      sb.delete(sb.length - 2 , sb.length)
      sb.append(s"): $clazzName = ").append(s"new $clazzName(\n")

      indent += "  "
      for {
        m <- fields
        mName = m.getName
        mType = theType(m)
      } sb.append(s"$indent$mName = $mName,\n")
      sb.delete(sb.length - 2 , sb.length)
      sb.append(")\n\n")

      // toString
      indent = "  "
      sb.append(s"${indent}override def toString =\n")
      indent += " " * 2
      sb.append(s"""${indent}s\"\"\"${clazzName}(""")
      for {
        m <- fields
        mName = m.getName
        mType = theType(m)
      } sb.append("$" + mName + ",")
      sb.delete(sb.length - 1 , sb.length)
      sb.append(s""")\"\"\"""".stripMargin)
      sb.append("\n")


      sb.append("}\n") // end class

      // companion object
      indent = "  "
      sb.append(s"object $clazzName {\n")
      sb.append(s"${indent}/** Scala API */\n")
      sb.append(s"${indent}def apply() = new $clazzName()\n")
      sb.append(s"${indent}/** Java API */\n")
      sb.append(s"${indent}def getInstance() = apply()\n")
      sb.append("}\n")

      sb.result()
    }

    private def theType(m: Field): String = {
      val raw = m.getGenericType.toString.replaceAll("<", "[").replaceAll(">", "]")
      raw match {
        case "boolean" | "int" | "long" | "byte" | "short" | "double" | "float" => up(raw)
        case name if name.startsWith("class ") => removeKnownPackages(name.replaceAll("class ", ""))
        case _ => removeKnownPackages(raw)
      }
    }

    private def removeKnownPackages(clsName: String): String = {

      @tailrec def removePkg(name: String, remaining: List[String]): String = remaining match {
        case Nil => name
        case pkg :: tail =>
          if (name.startsWith(pkg) && name.charAt(pkg.length).isUpper) name.drop(pkg.length)
          else removePkg(name, tail)
        }

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
      removeClass(clsName3, knownClasses)
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
