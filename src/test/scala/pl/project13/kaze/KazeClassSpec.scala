package pl.project13.kaze

import scala.concurrent.duration.FiniteDuration

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
           |  val items: List[Item],
           |  val opt: Option[String],
           |  val timeout: scala.concurrent.duration.FiniteDuration)
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
           |  def getOpt: java.util.Optional[String] = opt.asJava
           |  /** Java API */
           |  def getTimeout: java.time.Duration = timeout.asJava
           |
           |  def withName(value: String): Person = copy(name = value)
           |  def withAge(value: Int): Person = copy(age = value)
           |  def withItem(value: Item): Person = copy(item = value)
           |  /** Scala API */
           |  def withItems(values: List[Item]): Person = copy(items = values)
           |  /** Java API */
           |  def withItems(values: java.util.List[Item]): Person = copy(items = values.asScala.toList)
           |  def withOpt(value: String): Person = copy(opt = Option(value))
           |  /** Scala API */
           |  def withTimeout(value: scala.concurrent.duration.FiniteDuration): Person = copy(timeout = value)
           |  /** Java API */
           |  def withTimeout(value: java.time.Duration): Person =
           |    withTimeout(scala.concurrent.duration.FiniteDuration(value.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS))
           |
           |  private def copy(
           |    name: String = name,
           |    age: Int = age,
           |    item: Item = item,
           |    items: List[Item] = items,
           |    opt: Option[String] = opt,
           |    timeout: scala.concurrent.duration.FiniteDuration = timeout): Person = new Person(
           |      name = name,
           |      age = age,
           |      item = item,
           |      items = items,
           |      opt = opt,
           |      timeout = timeout)
           |
           |  override def toString =
           |    s```Person(name=$name,age=$age,item=$item,items=$items,opt=$opt,timeout=$timeout)```
           |
           |  override def equals(other: Any): Boolean = other match {
           |    case that: Person =>
           |      java.util.Objects.equals(this.name, that.name) &&
           |      java.util.Objects.equals(this.age, that.age) &&
           |      java.util.Objects.equals(this.item, that.item) &&
           |      java.util.Objects.equals(this.items, that.items) &&
           |      java.util.Objects.equals(this.opt, that.opt) &&
           |      java.util.Objects.equals(this.timeout, that.timeout)
           |        case _ => false
           |  }
           |
           |  override def hashCode(): Int =
           |    java.util.Objects.hash(
           |      name,
           |      Int.box(age),
           |      item,
           |      items,
           |      opt,
           |      timeout)
           |}
           |object Person {
           |  /** Scala API */
           |  def apply(): Person = new Person()
           |  /** Java API */
           |  def getInstance(): Person = apply()
           |  /** Scala API */
           |  def apply(
           |    name: String,
           |    age: Int,
           |    item: Item,
           |    items: List[Item],
           |    opt: Option[String],
           |    timeout: scala.concurrent.duration.FiniteDuration
           |  ): Person = new Person(
           |    name,
           |    age,
           |    item,
           |    items,
           |    opt,
           |    timeout
           |  )
           |
           |  /** Java API */
           |  def create(
           |    name: String,
           |    age: Int,
           |    item: Item,
           |    items: java.util.List[Item],
           |    opt: java.util.Optional[String],
           |    timeout: java.time.Duration
           |  ): Person = new Person(
           |    name,
           |    age,
           |    item,
           |    items.asScala.toList,
           |    opt.asScala,
           |    timeout.asScala
           |  )
           |}""".stripMargin.replaceAll("```", "\"\"\"").split("\n")

      info("Rendered: \n" + rendered)

      rendered.split("\n").zipWithIndex.foreach { case (renderedLine, idx) =>
        withClue(s"line=${idx+1}") {
          renderedLine should ===(expected(idx))
        }
      }
    }
  }
}

trait Extendable

case class Person(name: String, age: Int, item: Item, items: List[Item], opt: Option[String], timeout: FiniteDuration) extends Extendable

class Item

