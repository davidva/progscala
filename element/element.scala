object Element {
  def element(value: String) =
    new LineElement(value)

  def main(args: Array[String]) {
    val column1 = element("hello") above element("***")
    val column2 = element("***") above element("world")
    println(column1 beside column2)
  }
}

abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length
  def above(that: Element): Element =
    new ArrayElement(this.contents ++ that.contents)
  def beside(that: Element): Element = {
    val contents = new Array[String](this.contents.length)
    for (i <- 0 until this.contents.length)
      contents(i) = this.contents(i) + that.contents(i)
    new ArrayElement(contents)
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
