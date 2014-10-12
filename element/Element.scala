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
    println(elem("hello") above elem("world!"))
    println(elem(Array("one", "two")) beside elem("one"))
  }
}

import Element.elem

abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length
  def above(that: Element): Element = {
    val this1 = this widen that.width
    val that1 = that widen this.width
    elem(this1.contents ++ that1.contents)
  }
  def beside(that: Element): Element = {
    val this1 = this heighten that.height
    val that1 = that heighten this.height
    elem(
      for ((line1, line2) <- this1.contents zip that1.contents)
      yield line1 + line2
    )
  }
  def widen(w: Integer) = {
    if (width <= w) this
    else {
      val left = elem(' ', (w - width) /2, height)
      val right = elem(' ', w - width - left.width, height)
      left beside this beside right
    }
  }
  def heighten(h: Integer) = {
    if (h <= height) this
    else {
      val top = elem(' ', width, (h -height) / 2)
      val bot = elem(' ', width, h - height - top.height)
      top above this above bot
    }
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
