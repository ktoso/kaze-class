package pl.project13.kaze

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
           |  val opt: Option[String]) {
           |
           |  def withName(value: String): Person = copy(name = value)
           |  def withAge(value: Int): Person = copy(age = value)
           |  def withItem(value: Item): Person = copy(item = value)
           |  def withItems(value: List[Item]): Person = copy(items = value)
           |  def withOpt(value: String): Person = copy(opt = Option(value))
           |
           |  private def copy(
           |    name: String = name,
           |    age: Int = age,
           |    item: Item = item,
           |    items: List[Item] = items,
           |    opt: Option[String] = opt): Person = new Person(
           |      name = name,
           |      age = age,
           |      item = item,
           |      items = items,
           |      opt = opt)
           |
           |  override def toString =
           |    s```Person($name,$age,$item,$items,$opt)```
           |}
           |object Person {
           |  /** Scala API */
           |  def apply() = new Person()
           |  /** Java API */
           |  def getInstance() = apply()
           |}""".stripMargin.replaceAll("```", "\"\"\"").split("\n")

      info("Rendered: \n" + rendered)

      rendered.split("\n").zipWithIndex.foreach { case (renderedLine, idx) =>
        renderedLine should === (expected(idx))
      }
    }
  }
}

case class Person(name: String, age: Int, item: Item, items: List[Item], opt: Option[String])

class Item
