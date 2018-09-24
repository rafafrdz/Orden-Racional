

```scala
class Racional(num0 : Int, den0: Int) extends Ordered[Racional]{
  private def maxComDiv(x:Int,y:Int):Int = {
    def aux(ac:Int,n:Int):Int = {
      if(n==0) ac
      else aux(n,ac%n)
    }
    aux(x,y)
  }
  private val mcd = maxComDiv(num0,den0)
  val num =if(num0>0 && den0<0 || num0<0 && den0>0) -num0 / mcd else num0 / mcd
  val den =if(den0==0) sys.error("No se puede") else den0 / mcd

  override def toString :String = "Racional(%d,%d)".format(num,den)
  override def equals(that:Any):Boolean =
    that match {
      case pt:Racional => return(pt.num*den==num*pt.den )
      case _ => return false
    }
  def +(that:Racional):Racional = new Racional(num*that.den + that.num*den,den*that.den)
  def -(that:Racional):Racional = new Racional(num*that.den - that.num*den,den*that.den)
  def *(that:Racional):Racional = new Racional(num*that.num,den*that.den)
  def /(that:Racional):Racional = new Racional(num*that.den,den*that.num)

  def compare(that:Racional):Int={
    if(den==that.den) num.compare(that.num)
    else (num*that.den).compare(den*that.num)
  }
}
object racionales extends App {
  val r1 = new Racional(1,3)
  val r2 = new Racional(4,7)
  val r3 = new Racional(6,7)
  val rs = List(r1,r2,r3)
  println(rs.max)
  println(rs.min)
  if(r1 < r2) println(r1+" es menor que "+r2)
  else println(r2+" es menor que "+r1)
}
```



