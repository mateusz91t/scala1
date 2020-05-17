package ex1

import scala.annotation.tailrec

object ex1 {
  //val - stała/wartość
  //var - zmienna
  def main(args: Array[String]) {
    println("NBD 1.1a")
    var dni = List("poniedziałek","wtorek","środa","czwartek","piątek","sobota","niedziela")
    var dniRazem1a = "";
    val forDni = for (dzien <- dni) {
      dniRazem1a += dzien + ", "
    }
    dniRazem1a foreach print
    println()

    println()
    println("NBD 1.1b")
    var dniRazem1b = "";
    for (i <- 0 until dni.length if dni(i).startsWith("p")){
      dniRazem1b += dni(i) +", "
    }
    println(dniRazem1b)

    println()
    println("NBD 1.1c")
    var dniRazem1c = ""
    var iter: Int = 0 // da się to zrobić bez iteratora? Jakoś z head/tail?
    while(iter < dni.length){
      dniRazem1c += dni(iter) + ", "
      iter = iter + 1
    }
    println(dniRazem1c)

    println()
    println("NBD 1.2a")
    def dniReku2a (listaDni: List[String]) : String = {
//      są te 2x def, aby ukryć w środku tego vara dniRazem2a. Jeśli zostawię go w d1 to będzie się nullować co iterację
      var dniRazem2a = ""
      def d1(listaDni: List[String]) : String = {
        if(!listaDni.isEmpty){
          dniRazem2a += listaDni.head + ", "
          d1(listaDni.tail)
        }
        else dniRazem2a
      }
      d1(listaDni)
      dniRazem2a
    }
    println(dniReku2a(dni))

    println()
    println("NBD 1.2b")
    var dniRazem2b = ""
    def dniReku2b (listaDni: List[String]) : String ={
      if(!listaDni.isEmpty){
        dniRazem2b += listaDni.reverse.head + ", "
        dniReku2b(listaDni.reverse.tail.reverse)
        /*
        muszę przekazać bez ostatniego el., ale żeby rozpoczynając kolejną iterację lista nie była odwrócona,
        więc odwracam listę,
        zabieram ogon (odcinam head),
        znów odwracam
         */
      }
      else dniRazem2b
    }
    println(dniReku2b(dni))

    println()
    println("NBD 1.3")
    def dniReku3(listaDni: List[String]) : String = {
      @tailrec
      def d1(list: List[String], concatEl: String) : String = {
        if(list.isEmpty){
          concatEl
        }
          else d1(list.tail,concatEl + list.head + ", ")
      }
      d1(listaDni,"")
    }
    println(dniReku3(dni))

    println()
    println("NBD 1.4a")
    val str4a : String = dni.foldLeft("")(_ + _ + ", ")
    println(str4a)

    println()
    println("NBD 1.4b")
    val str4b = dni.foldRight("")(_ + ", " + _)
    println(str4b)

    println()
    println("NBD 1.4c")
    val str4c = dni.fold("")((a, b) => a + b + ", ")
    val str4c_v2 = dni.fold("")(_ + _ + ", ")
    println(str4c)
    println(str4c_v2)

    println()
    println("NBD 1.5")
    val products5 = Map("fish" -> 5.0, "bread" -> 1.0, "milk" -> 2.0, "chocolate" -> 2.5)
    val products5_prom = products5 map {case (k,v) => (k, 0.9 * v)}
    print("products5: "); println(products5)
    print("products5_prom: "); println(products5_prom)

    println()
    println("NBD 1.6")
    val krotka6 = Tuple3(2d,3,true)
    def prtKrotka6(tuple3: Tuple3[_,_,_]): String = {
      tuple3._1 + tuple3._2.toString + tuple3._3
    }
    println(prtKrotka6(krotka6))

    println()
    println("NBD 1.7")
    val map7 = Map(1->"one",3->"three",5->"five",7->"seven",9->"nine")
    println(map7.getOrElse(3,0))
    println(map7.getOrElse(2,0))

    println()
    println("NBD 1.8")
    val list8 = List(1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,0)
    def returnWithout0(list_0: List[Int]):List[Int]={
      @tailrec
      def d2(list: List[Int], returnList: List[Int]):List[Int]={
        if(list.nonEmpty) {
          val t1 = list.head
          if (t1 != 0) {
            d2(list.tail,t1 :: returnList)
          }
          else d2(list.tail,returnList)
        }
        else returnList.reverse
      }
      d2(list_0,List[Int]())
    }
    println(returnWithout0(list8))

    println()
    println("NBD 1.9")
    val list9 = List(1,2,3,4)
    def listPlusPlus(list: List[Int]):List[Int]={
      list map (_ +1)
    }
    println(listPlusPlus(list9))

    println()
    println("NBD 1.10")
    var list10 = List[Int]()
    var iter10 = -10
    while(iter10 <= 20){
      val oddNumber = iter10 % 2
      if(oddNumber == 0)
        iter10 :: list10
      else
        iter10 * -1 :: list10
      iter10 = iter10+1
    }

    println(list10)
  }
}