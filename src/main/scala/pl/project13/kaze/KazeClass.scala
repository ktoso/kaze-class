package pl.project13.kaze

object KazeClass {
  import scala.reflect.ClassTag

  def of[T](implicit tag: ClassTag[T]) =
    new MkKazeClazz(tag.runtimeClass)

  class MkKazeClazz(clazz: Class[_]) {
    import java.lang.reflect.Method

    private var indent = ""

    private val name = simpleName(clazz)

    private val skipThose = Set(
      "apply", "unapply", "curried", "tupled",
      "canEqual", "equals", "toString", "hashCode", "copy",
      "productIterator", "productPrefix", "productArity", "productElement"
    )
    private val methods = clazz.getDeclaredMethods
        .filterNot(skip)
        .filterNot(_.getName.contains("$"))
        .sortBy(_.getName)

    def render = {
      val sb = new StringBuilder(s"${indent}final class $name private(\n")
      indent = "  "

      // constructor

      for {
        m <- methods
        mName = m.getName
        mType = theType(m)
      } sb.append(s"${indent}val $mName: $mType,\n")
      sb.delete(sb.length - 2 , sb.length)
      sb.append(") {\n")
      sb.append("\n")

      // with...

      for {
        m <- methods
        mName = m.getName
        mType = theType(m)
      } sb.append(s"${indent}def with${up(mName)}(value: $mType): $name = copy($mName = value)\n")

      sb.append("\n")

      // copy method

      sb.append(s"${indent}private def copy(\n")
      indent += "  "
      for {
        m <- methods
        mName = m.getName
        mType = theType(m)
      } sb.append(s"$indent$mName: $mType = $mName,\n")
      sb.delete(sb.length - 2 , sb.length)
      sb.append(s"): $name = ").append(s"new $name(\n")

      indent += "  "
      for {
        m <- methods
        mName = m.getName
        mType = theType(m)
      } sb.append(s"$indent$mName = $mName,\n")
      sb.delete(sb.length - 2 , sb.length)
      sb.append(")\n\n")

      // toString
      indent = "  "
      sb.append(s"${indent}override def toString =\n")
      indent += " " * 2
      sb.append(s"""${indent}s\"\"\"${name}(""")
      for {
        m <- methods
        mName = m.getName
        mType = theType(m)
      } sb.append("${" + mName + "},")
      sb.delete(sb.length - 1 , sb.length)
      sb.append(s""")\"\"\")""".stripMargin)
      sb.append("\n")


      sb.append("}\n") // end class

      // companion object
      indent = "  "
      sb.append(s"object $name {\n")
      sb.append(s"${indent}def apply() = new $name()\n")
      sb.append(s"${indent}/** Java API */\n")
      sb.append(s"${indent}def getInstance() = apply()\n")
      sb.append("}\n")

      sb.result()
    }

    private def theType(m: Method): String = {
      val raw = m.getGenericReturnType.toString.replaceAll("<", "[").replaceAll(">", "]")
      raw match {
        case "boolean" | "int" | "long" | "byte" | "short" | "double" | "float" => up(raw)
        case name if name.startsWith("class ") => name.replaceAll("class ", "")
        case _ => raw
      }
    }

    private def skip(m: Method) = skipThose(m.getName)

    private def simpleName(clazz: Class[_]): String = {
      val n = clazz.getName
      val i = n.lastIndexOf('.')
      n.substring(i + 1)
    }

    private def up(s: String): String = s.charAt(0).toUpper + s.tail
  }

}