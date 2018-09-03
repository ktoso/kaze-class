package pl.project13.kaze

import scala.concurrent.duration.FiniteDuration
import scala.collection.immutable

import org.scalatest.{Matchers, WordSpec}

class KazeClassSpec extends WordSpec with Matchers {

  "KazeClass" should {
    "make simple class into kaze-class" in {
      val rendered: String = KazeClass.of[Person].render

      val expected =
        """|final class Person private(
           |  val name: String,
           |  val age: Int,
           |  val item: Item,
           |  val items: immutable.Seq[Item],
           |  val list: List[Item],
           |  val opt: Option[String],
           |  val timeout: scala.concurrent.duration.FiniteDuration,
           |  val flag: Boolean)
           |extends pl.project13.kaze.Extendable {
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
           |  def getTimeout: java.time.Duration = timeout.asJava
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
           |  def withTimeout(value: java.time.Duration): Person =
           |    withTimeout(scala.concurrent.duration.FiniteDuration(value.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS))
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
           |    flag: Boolean = flag): Person = new Person(
           |      name = name,
           |      age = age,
           |      item = item,
           |      items = items,
           |      list = list,
           |      opt = opt,
           |      timeout = timeout,
           |      flag = flag)
           |
           |  override def toString =
           |    s```Person(name=$name,age=$age,item=$item,items=$items,list=$list,opt=$opt,timeout=$timeout,flag=$flag)```
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

      info("Rendered: \n" + rendered)

      rendered.split("\n").zipWithIndex.foreach { case (renderedLine, idx) =>
        withClue(s"line=${idx + 1}") {
          renderedLine should ===(expected(idx))
        }
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

