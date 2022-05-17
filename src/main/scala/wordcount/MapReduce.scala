package wordcount
import java.text.SimpleDateFormat

object MapReduce {

  val dateFormatPattern= "yyyy.MM.dd G 'at' HH:mm:ss z"
  val sdf= new SimpleDateFormat(dateFormatPattern)

  def mapReduceKV[S,B,R](mapFun:(S=>List[B]), redFun:(R,B)=>R, base:R, l:List[S]):R =
    l.flatMap(mapFun).
      foldLeft[R](base)(redFun)

  /**********************************************************************************************
   *
   *                          Aufgabe 4
   *
   *********************************************************************************************
   */

  /*
    Write a function that determines how many jobs each user sumbmitted
    Result: Map (key:user, value: number)
   */
  def numberOfJobsPerUser(l:List[(String,String,String,Int)]):Map[String,Int]= {

    mapReduceKV[(String, String, String, Int), (String, Int), Map[String,Int]] (
      x => List((x._2, 1)),
      (map, x) => map.updated(x._1, x._2 + map.getOrElse(x._1, 0)),
      Map():Map[String,Int],
      l
    )
  }

  /*
  Write a function that determines how many times a job name was used from each user
  Result: Map (key:(user,Job), value: number)
 */
  def numberOfJobsPerUserUsingACertainName(l:List[(String,String,String,Int)]):Map[(String,String),Int]= {

    mapReduceKV[(String, String, String, Int), (String, String, Int), Map[(String,String),Int]] (
      x => List((x._2, x._3, 1)),
      (map, x) => map.updated((x._1, x._2), x._3 + map.getOrElse((x._1, x._2), 0)),
      Map():Map[(String,String),Int],
      l
    )


  }

  /*
    Write a function that determines all job names (without duplicates)
    Result: List(jobnames)
*/
  def distinctNamesOfJobs(l:List[(String,String,String,Int)]):List[String]= {

    mapReduceKV[(String, String, String, Int), (String), List[String]] (
      x => List(x._3),
      (list, x) => if(!list.contains(x)) x :: list else list,
      List(): List[String],
      l
    )
  }

  /*
    Write a function that determines how many jobs lasted more than 20sec
    Result: Map (key:("more" or "less"), value: number)
  */
  def moreThanXSeconds(l:List[(String,String,String,Int)]):Map[String,Int]= {

    mapReduceKV[(String, String, String, Int), (String, Int), Map[String,Int]] (
      x => List((if(x._4 > 20) "more" else "less", 1)),
      (map, x) => map.updated(x._1, x._2 + map.getOrElse(x._1, 0)),
      Map():Map[String,Int],
      l
    )

  }

  /*
    Write a function that determines the number of were submitted per day
    Result: Map (key:day- format "YYYY-MM-dd" , value: number)
  */
  def numberOfJobsPerDay(l:List[(String,String,String,Int)]):Map[String,Int]= {

    mapReduceKV[(String, String, String, Int), (String, Int), Map[String, Int]] (
      x => List((x._1.replace(".", "-" ).slice(0, 10), 1)),
      (map, x) => map.updated(x._1, x._2 + map.getOrElse(x._1, 0)),
      Map():Map[String,Int],
      l
    )

  }
}
