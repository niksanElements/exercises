
sealed trait Shape
case class Triangle(a: Int, b: Int, c: Int, h: Int) extends Shape // h represent the height against the longest side of the triangle
case class Rectangle(a: Int, b: Int) extends Shape
case class Trapezoid(a: Int, b: Int, h: Int) extends Shape
case class Cube() extends Shape

class Architect {

  /*
   *  Finds the max element from given list of integers.
   *  The result is wrapped in an Option instance. The Option has two forms:
   *   - None if no element satisfies the search criteria (for example, in case an empty list is provided)
   *   - Some(x), where x if the searched element. In this case, it can be acquired with the method get. Example:
   *     val o: Option[Int] = Some(6)
   *     val n: Int = o.get
   */
  def max(xs: List[Int]): Option[Int] = {
    def loop(in:List[Int],currentMax: Int): Int = {
      if(in.size != 0) {
        if(currentMax < in.head)
          return loop(in.tail,in.head)
        else
          return loop(in.tail,currentMax)
      }
      return currentMax;
    }

    val maxEl = loop(xs,0)
    val result = Option[Int](maxEl)
    result
  }

  // Determines the type of given triangle: "rectangular", "equilateral", "isosceles", "random"
  def triangleType(t: Triangle): String = {
    if(t.a == t.b == t.c) return "equilateral"
    if(t.a == t.b) return"isosceles"
    if((t.a*t.a+t.b*t.b) == (t.c*t.c)) return "rectangular"
    return "random"
  }

  /*
   * Calculates the area of the provided shape, by using these formulae:
   *  - Rectangular triangle: a * b / 2, where a and b are cathetus
   *  - Any triangle except rectangular: x * h / 2, where x is the largest side of the triangle and h is the opposite height
   *  - Rectangle: a * b, where a and b are both sides
   *  - Trapezoid: (a + b) * h / 2, where a and b are the parallel sides and h is the height between them
   *  - Cube: always return -1
   *  
   *  Hint: for triangles use the max function
   */
  def area(s: Shape): Double = s match{
    case x: Trianlge =>{
      if(triangleType(x) == "rectangular") return x.a* x.b/2.0
      else{
        val largestSide = max(List[Int](x.a,x.b,x.c))
        return largestSide * x.h/2.0
      }
    }
    case x: Rectangle => x.a * x.b
    case x: Trapezoid => (x.a + x.b)*x.h/2.0
    case x: Cube => -1
  }

  /*
   *  Returns the number of rectangular triangles in given list of shapes
   *  
   *  Hint: use the triangleType function
   */
  def findRectangulars(shapes: List[Shape]): Int = {
    def iter(shapes: List[Shape], n: Int): Int = ???
    iter(???, ???)
  }
}

object test{
  def main (args: Array[String]){
    val architect = new Architect();

    val list = List[Int](1,2,3,4,6,2,4,1,6,8,3)
    println(architect.max(list).get)

    val triangle = Triangle(15,2,3,4)

    println(architect.area(triangle))
  }
}

