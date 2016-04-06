package pl.project13.kaze

import org.scalatest.{Matchers, WordSpec}

class KazeClassSpec extends WordSpec with Matchers {

  "KazeClass" should {
    "make simple class into kaze-class" in {
      val rendered: String = KazeClass.of[Person].render

      val expected =
        """|final class Person private(
           |  name: class java.lang.String,
           |  items: scala.collection.immutable.List[pl.project13.kaze.Item],
           |  age: Int) {
           |
           |  def withName(value: class java.lang.String): Person = copy(name = value)
           |  def withItems(value: scala.collection.immutable.List[pl.project13.kaze.Item]): Person = copy(items = value)
           |  def withAge(value: Int): Person = copy(age = value)
           |
           |  private def copy(
           |    name: class java.lang.String = name,
           |    items: scala.collection.immutable.List[pl.project13.kaze.Item] = items,
           |    age: Int = age): Person = new Person(
           |      name = name,
           |      items = items,
           |      age = age)
           |}
           |object Person {
           |  def apply() = new Person()
           |  /** Java API */
           |  def getInstance() = apply()
           |}""".stripMargin.split("\n")

      info("Rendered: \n" + rendered)

      rendered.split("\n").zipWithIndex.foreach { case (renderedLine, idx) =>
        renderedLine should === (expected(idx))
      }
    }
  }
}

case class Person(name: String, age: Int, items: List[Item])

class Item
