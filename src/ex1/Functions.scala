// package ex1

import scala.collection.mutable.Stack

object Functions {

  // Връща големината на масив (без да ползва data.length!!!)
  def length(data: List[Int]): Int = if(data.size != 1) 1 + length(data.tail) else 1

  // Ако cond е true връща onTrue
  def ifelse(cond: Boolean, onTrue: Int, onFalse: Int) = if(cond) onTrue else onFalse

  // Проверява дали скобите в даден масив от символи са балансирани.
  // Коректно: (a)asda(b)(v) | (((a))) | ()(()асдасд)
  // Грешно: )() | ((д) | ((das) (d)( 
  def balance(chars: List[Char]): Boolean = {
    
    var s = Stack[Char]();
    var isBalance = true;
    
    def check(a: Char,b: Char) = {if(a == '(' && b == ')') true else false}
    def check_is_brack(a: Char) = if ( a == '(' || a == ')') true else false
    
    for(ch <- chars){
      if(!s.isEmpty && check(s.top,ch))
        s.pop()
      else if(check_is_brack(ch))
        s.push(ch)
    }
    
    if(s.isEmpty) true else false
  }

  def map(chars: List[Char], f: Any) =  ???

  def toUpperCase(chars: List[Char]) = {
    def upperCase(char: Char) :Char = char match{
      case 'a' => 'A'; case 'b' => 'B'; case 'c' => 'C'; case 'd' => 'D'; case 'E' => 'A';
      case 'f' => 'F'; case 'g' => 'G'; case 'h' => 'H'; case 'i' => 'I'; case 'j' => 'J';
      case 'k' => 'K'; case 'l' => 'L'; case 'm' => 'M'; case 'n' => 'N'; case 'o' => 'O';
      case 'p' => 'P'; case 'q' => 'Q'; case 'r' => 'R'; case 's' => 'S'; case 't' => 'T';
      case 'u' => 'U'; case 'v' => 'V'; case 'w' => 'W'; case 'x' => 'X'; case 'y' => 'Y';
      case 'z' => 'Z'; case x => x;
    }
  
    chars.map(upperCase(_))
  }

  // Проверява дали съществува елемент отговарящ на f
  def exists(data: List[Int], f: Any) : Boolean = f match{
    case x: Int => 
      if(data.size == 0) return false
      else if(x == data.head) return true 
      exists(data.tail,f) 
    case x: Any => false
  }

  // Връща масив съдържащ само елементите отговарящи на f
  def filter(data: List[Int], f: Any) : List[Int] = f match {
    case x: Int =>{
      def loop(in: List[Int],out:List[Int],d:Int) : List[Int] = {
        if (in.size == 0) out
        else loop(in.tail,if(in.head == d) out :+ d else out,d)
      }
      loop(data,List(),x)
    }
    case x: Any => List()
  }
  // Проверява дали всички елементи отговарят на f
  def forall(data: List[Int], f: Any) : Boolean = ???

  // Връща числото от триъгълника на Паскал отговарящо на съответния ред/колона
  def pascal(c: Int, r: Int): Int = {
    if(c == 0) return 0
    if(r == 1 && c == 1) return 1
    if(c > r) return 0
    return pascal(c - 1,r - 1) + pascal(c,r - 1)
  }

// test method
  def main(args: Array[String]){
    
    val nums = List(1,2,2,4,3,3,3,3,4,5,6,7,3,3,3);

    println(length(nums));

    val chars = List('a','b','V','r','R','p')
    toUpperCase(chars).foreach(println)

    println(exists(nums,4))
    println(exists(nums,10))
    println(exists(nums,'3'))
    
    filter(nums,3).foreach(println)
    println(".....  TEST PASCAL .....")
    println(pascal(1,1)) 
    println(pascal(3,5)) 
    println(pascal(5,7)) 
    println(pascal(8,9)) 
    println(pascal(0,0))  
  }
}
