package pl.project13.kaze

import org.scalatest.{Matchers, WordSpec}

class KazeClassSpec extends WordSpec with Matchers {

  "KazeClass" should {
    "make simple class into kaze-class" in {
      val rendered: String = KazeClass.of[Person].render

      val expected =
        """|final class Person private(
           |  val name: java.lang.String,
           |  val age: Int,
           |  val items: scala.collection.immutable.List[pl.project13.kaze.Item]) {
           |
           |  def withName(value: java.lang.String): Person = copy(name = value)
           |  def withAge(value: Int): Person = copy(age = value)
           |  def withItems(value: scala.collection.immutable.List[pl.project13.kaze.Item]): Person = copy(items = value)
           |
           |  private def copy(
           |    name: java.lang.String = name,
           |    age: Int = age,
           |    items: scala.collection.immutable.List[pl.project13.kaze.Item] = items): Person = new Person(
           |      name = name,
           |      age = age,
           |      items = items)
           |
           |  override def toString =
           |    s```Person(${name},${age},${items})```
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

case class Person(name: String, age: Int, items: List[Item])

class Item
