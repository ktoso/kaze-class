package pl.project13.kaze

import org.scalatest.{Matchers, WordSpec}

class KazeClassSpec extends WordSpec with Matchers {

  "KazeClass" should {
    "make simple class into kaze-class" in {
      val rendered: String = KazeClass.of[Person].render

      val expected =
        """|final class Person private(
           |  age: Int,
           |  items: scala.collection.immutable.List[pl.project13.kaze.Item],
           |  name: class java.lang.String) {
           |
           |  def withAge(value: Int): Person = copy(age = value)
           |  def withItems(value: scala.collection.immutable.List[pl.project13.kaze.Item]): Person = copy(items = value)
           |  def withName(value: class java.lang.String): Person = copy(name = value)
           |
           |  private def copy(
           |    age: Int = age,
           |    items: scala.collection.immutable.List[pl.project13.kaze.Item] = items,
           |    name: class java.lang.String = name): Person = new Person(
           |      age = age,
           |      items = items,
           |      name = name)
           |
           |  override def toString =
           |    s```Person(${age},${items},${name})```)
           |}
           |object Person {
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
