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
           |  val timeout: scala.concurrent.duration.FiniteDuration) {
           |
           |  def withName(value: String): Person = copy(name = value)
           |  def withAge(value: Int): Person = copy(age = value)
           |  def withItem(value: Item): Person = copy(item = value)
           |  def withItems(value: List[Item]): Person = copy(items = value)
           |  def withOpt(value: String): Person = copy(opt = Option(value))
           |  def withTimeout(value: scala.concurrent.duration.FiniteDuration): Person = copy(timeout = value)
           |  def withTimeout(value: java.time.Duration): Person =
           |    withTimeout(scala.concurrent.duration.FiniteDuration.create(value.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS))
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
           |    s```Person($name,$age,$item,$items,$opt,$timeout)```
           |}
           |object Person {
           |  /** Scala API */
           |  def apply() = new Person()
           |  /** Java API */
           |  def getInstance() = apply()
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

case class Person(name: String, age: Int, item: Item, items: List[Item], opt: Option[String], timeout: FiniteDuration)

class Item
