object Element {
  def elem(line: String): Element =
    new LineElement(line)
  def elem(ch: Char, width: Int, height: Int): Element =
    new UniformElement(ch, width, height)
  def elem(contents: Array[String]): Element =
    new ArrayElement(contents)

  def main(args: Array[String]) {
    val column1 = elem("hello") above elem("***")
    val column2 = elem("***") above elem("world")
    println(column1 beside column2)
  }
}

import Element.elem

abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length
  def above(that: Element): Element =
    elem(this.contents ++ that.contents)
  def beside(that: Element): Element = {
    elem(
      for (
        (line1, line2) <- this.contents zip that.contents
      ) yield line1 + line2
    )
  }
  override def toString = contents mkString "\n"
}

class ArrayElement(
  val contents: Array[String]
) extends Element

class LineElement(s: String) extends Element {
  val contents = Array(s)
  override def width = s.length
  override def height = 1
}

class UniformElement(
  ch: Char,
  override val width: Int,
  override val height: Int
) extends Element {
  private val line = ch.toString * width
  def contents = Array.fill(height)(line)
}
