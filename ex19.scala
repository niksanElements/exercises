class Property(City: String,Kv: Double, Price: Double);

class House(City: String,Kv: Double,Price: Double,GardenKv: Double) extends Property(City,Kv,Price);

class Garaje(City: String,Kv: Double,Price: Double,IsInSide: Boolean) extends Property(City,Kv,Price);

object store {

  def decompose(str: String) : List[Char] = {
    def loop(str:String,i:Int,result: List[Char]): List[Char] = {
      if(i < 0) return result;
      else loop(str,i - 1,str.charAt(i) :: result);
    }
    
    loop(str,str.length() - 1,List());
  }
  
  def takeWhile(data: List[String], f :String => Boolean): List[String] = {
    def loop(data:List[String],result:List[String]): List[String] = data match{
      case Nil => return result
      case head::tail => if(f(head)) loop(tail,head :: result); else return result;
    }
    
    loop(data,List());
  }
  
  def is(prop:Property): Int = prop match{
    case _:House => return 2;
    case _:Garaje => return 3;
    case _:Property => return 1;
    case _ => return -1
  }
  def main(args: Array[String]): Unit = {

    val prop = List(
        new Property("Sofia",130.5,400000),
        new Property("Sofia",145.4,500000),
        new Property("Vidin",500.5,7000000),
        new House("Sofia",40.6,4000000,20.4),
        new House("Vidin",50.4,320000,15.6),
        new House("Ruse",23.4,50000,10.4),
        new Garaje("Ruse",35.5,60534,true),
        new Garaje("Varna",34.54,2353223,false),
        new Garaje("Varna",234.45,3458,true))
      prop.foldLeft(\)(op)
      
   
  }
}
