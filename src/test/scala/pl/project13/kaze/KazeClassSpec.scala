package pl.project13.kaze

import scala.concurrent.duration.FiniteDuration
import scala.collection.immutable

import org.scalatest.{Matchers, WordSpec}

class KazeClassSpec extends WordSpec with Matchers {

  "KazeClass" should {
    "make simple class into kaze-class" in {
      val rendered: String = KazeClass.of[Person].withoutAkkaUtils.withEqualsHashcode.render

      val expected =
        """|import scala.collection.immutable
           |import scala.collection.JavaConverters._
           |import scala.compat.java8.OptionConverters._
           |import scala.compat.java8.DurationConverters
           |
           |final class Person private(
           |  val name: String,
           |  val age: Int,
           |  val item: Item,
           |  val items: immutable.Seq[Item],
           |  val list: List[Item],
           |  val opt: Option[String],
           |  val timeout: scala.concurrent.duration.FiniteDuration,
           |  val flag: Boolean
           |) extends pl.project13.kaze.Extendable {
           |
           |  /** Java API */
           |  def getName: String = name
           |  /** Java API */
           |  def getAge: Int = age
           |  /** Java API */
           |  def getItem: Item = item
           |  /** Java API */
           |  def getItems: java.util.List[Item] = items.asJava
           |  /** Java API */
           |  def getList: java.util.List[Item] = list.asJava
           |  /** Java API */
           |  def getOpt: java.util.Optional[String] = opt.asJava
           |  /** Java API */
           |  def getTimeout: java.time.Duration = timeout.toJava
           |  /** Java API */
           |  def isFlag: Boolean = flag
           |
           |  def withName(value: String): Person = copy(name = value)
           |  def withAge(value: Int): Person = copy(age = value)
           |  def withItem(value: Item): Person = copy(item = value)
           |  /** Scala API */
           |  def withItems(values: immutable.Seq[Item]): Person = copy(items = values)
           |  /** Java API */
           |  def withItems(values: java.util.List[Item]): Person = copy(items = values.asScala.toList)
           |  /** Scala API */
           |  def withList(values: List[Item]): Person = copy(list = values)
           |  /** Java API */
           |  def withList(values: java.util.List[Item]): Person = copy(list = values.asScala.toList)
           |  def withOpt(value: String): Person = copy(opt = Option(value))
           |  /** Scala API */
           |  def withTimeout(value: scala.concurrent.duration.FiniteDuration): Person = copy(timeout = value)
           |  /** Java API */
           |  def withTimeout(value: java.time.Duration): Person = copy(timeout = value.toScala)
           |  def withFlag(value: Boolean): Person = if (flag == value) this else copy(flag = value)
           |
           |  private def copy(
           |    name: String = name,
           |    age: Int = age,
           |    item: Item = item,
           |    items: immutable.Seq[Item] = items,
           |    list: List[Item] = list,
           |    opt: Option[String] = opt,
           |    timeout: scala.concurrent.duration.FiniteDuration = timeout,
           |    flag: Boolean = flag
           |  ): Person = new Person(
           |      name = name,
           |      age = age,
           |      item = item,
           |      items = items,
           |      list = list,
           |      opt = opt,
           |      timeout = timeout,
           |      flag = flag
           |    )
           |
           |  override def toString =
           |    "Person(" +
           |    s"name=$name," +
           |    s"age=$age," +
           |    s"item=$item," +
           |    s"items=$items," +
           |    s"list=$list," +
           |    s"opt=$opt," +
           |    s"timeout=${timeout.toCoarsest}," +
           |    s"flag=$flag" +
           |    ")"
           |
           |  override def equals(other: Any): Boolean = other match {
           |    case that: Person =>
           |      java.util.Objects.equals(this.name, that.name) &&
           |      java.util.Objects.equals(this.age, that.age) &&
           |      java.util.Objects.equals(this.item, that.item) &&
           |      java.util.Objects.equals(this.items, that.items) &&
           |      java.util.Objects.equals(this.list, that.list) &&
           |      java.util.Objects.equals(this.opt, that.opt) &&
           |      java.util.Objects.equals(this.timeout, that.timeout) &&
           |      java.util.Objects.equals(this.flag, that.flag)
           |        case _ => false
           |  }
           |
           |  override def hashCode(): Int =
           |    java.util.Objects.hash(
           |      name,
           |      Int.box(age),
           |      item,
           |      items,
           |      list,
           |      opt,
           |      timeout,
           |      Boolean.box(flag))
           |}
           |
           |object Person {
           |  /** Scala API */
           |  def apply(): Person = new Person()
           |  /** Java API */
           |  def create(): Person = apply()
           |  /** Scala API */
           |  def apply(
           |    name: String,
           |    age: Int,
           |    item: Item,
           |    items: immutable.Seq[Item],
           |    list: List[Item],
           |    opt: Option[String],
           |    timeout: scala.concurrent.duration.FiniteDuration,
           |    flag: Boolean
           |  ): Person = new Person(
           |    name,
           |    age,
           |    item,
           |    items,
           |    list,
           |    opt,
           |    timeout,
           |    flag
           |  )
           |
           |  /** Java API */
           |  def create(
           |    name: String,
           |    age: Int,
           |    item: Item,
           |    items: java.util.List[Item],
           |    list: java.util.List[Item],
           |    opt: java.util.Optional[String],
           |    timeout: java.time.Duration,
           |    flag: Boolean
           |  ): Person = new Person(
           |    name,
           |    age,
           |    item,
           |    items.asScala.toList,
           |    list.asScala.toList,
           |    opt.asScala,
           |    timeout.asScala,
           |    flag
           |  )
           |}""".stripMargin.replaceAll("```", "\"\"\"").split("\n")

      assertEquals(expected, rendered)
    }

    "create config helpers and no Java API" in {
      val rendered: String = KazeClass.of[Simple].withConfig.withoutJavaApi.render

      val expected =
        """import scala.collection.immutable
          |
          |final class Simple private(
          |  val number: Int,
          |  val optionalNumber: Option[optionalNumberErasedType],
          |  val text: String,
          |  val timeout: scala.concurrent.duration.FiniteDuration,
          |  val flag: Boolean,
          |  val ext: Extendable,
          |  val decimal: Double
          |) {
          |
          |  def withNumber(value: Int): Simple = copy(number = value)
          |  def withOptionalNumber(value: optionalNumberErasedType): Simple = copy(optionalNumber = Option(value))
          |  def withText(value: String): Simple = copy(text = value)
          |  /** Scala API */
          |  def withTimeout(value: scala.concurrent.duration.FiniteDuration): Simple = copy(timeout = value)
          |  def withFlag(value: Boolean): Simple = if (flag == value) this else copy(flag = value)
          |  def withExt(value: Extendable): Simple = copy(ext = value)
          |  def withDecimal(value: Double): Simple = copy(decimal = value)
          |
          |  private def copy(
          |    number: Int = number,
          |    optionalNumber: Option[optionalNumberErasedType] = optionalNumber,
          |    text: String = text,
          |    timeout: scala.concurrent.duration.FiniteDuration = timeout,
          |    flag: Boolean = flag,
          |    ext: Extendable = ext,
          |    decimal: Double = decimal
          |  ): Simple = new Simple(
          |      number = number,
          |      optionalNumber = optionalNumber,
          |      text = text,
          |      timeout = timeout,
          |      flag = flag,
          |      ext = ext,
          |      decimal = decimal
          |    )
          |
          |  override def toString =
          |    "Simple(" +
          |    s"number=$number," +
          |    s"optionalNumber=$optionalNumber," +
          |    s"text=$text," +
          |    s"timeout=${timeout.toCoarsest}," +
          |    s"flag=$flag," +
          |    s"ext=$ext," +
          |    s"decimal=$decimal" +
          |    ")"
          |}
          |
          |object Simple {
          |  /** Scala API */
          |  def apply(): Simple = new Simple()
          |  /**
          |   * Reads from the given config.
          |   */
          |  def apply(c: Config): Simple = {
          |    val number = c.getInt("number")
          |    val optionalNumber = c.get ("optional-number")
          |    val text = c.getString("text")
          |    val timeout = c.getDuration("timeout").asScala
          |    val flag = c.getBoolean("flag")
          |    val ext = c.get ("ext")
          |    val decimal = c.getDouble("decimal")
          |    new Simple(
          |      number,
          |      optionalNumber,
          |      text,
          |      timeout,
          |      flag,
          |      ext,
          |      decimal
          |    )
          |  }
          |
          |  /* sample config section
          |  simple {
          |    number = 1234567
          |    optional-number = ???
          |    text = "some text"
          |    timeout = 50 seconds
          |    flag = false
          |    ext = ???
          |    decimal = 1234.567
          |  }
          |  */
          |
          |  /** Scala API */
          |  def apply(
          |    number: Int,
          |    optionalNumber: Option[optionalNumberErasedType],
          |    text: String,
          |    timeout: scala.concurrent.duration.FiniteDuration,
          |    flag: Boolean,
          |    ext: Extendable,
          |    decimal: Double
          |  ): Simple = new Simple(
          |    number,
          |    optionalNumber,
          |    text,
          |    timeout,
          |    flag,
          |    ext,
          |    decimal
          |  )
          |
          |}
          |""".stripMargin.replaceAll("```", "\"\"\"").split("\n")

      assertEquals(expected, rendered)
    }
  }

  private def assertEquals(expected: Array[String], rendered: String) = {
    info("Rendered: \n" + rendered)
    rendered.split("\n").zipWithIndex.foreach { case (renderedLine, idx) =>
      withClue(s"line=${idx + 1}") {
        renderedLine should ===(expected(idx))
      }
    }
  }
}

trait Extendable

case class Person(name: String, age: Int, item: Item,
                  items: immutable.Seq[Item],
                  list: List[Item],
                  opt: Option[String], timeout: FiniteDuration,
                  flag: Boolean)
  extends Extendable

class Item

case class Simple(number: Int, optionalNumber: Option[Int], text: String, timeout: FiniteDuration, flag: Boolean, ext: Extendable, decimal: Double)
