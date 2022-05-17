package test

import java.text.SimpleDateFormat

import org.scalatest.funsuite.AnyFunSuite
import wordcount.MapReduce
import java.text.SimpleDateFormat

class MapReduceTest extends AnyFunSuite{

  val dateFormatPattern= "yyyy.MM.dd G 'at' HH:mm:ss z"
  val sdf= new SimpleDateFormat(dateFormatPattern)

  /*
      In diesem Anwendungsfall geht es um den Log-File eines Job-Schedulers
      Er beinhaltet die folgenden Elemente
     (<Datum und Uhrzeit>,<Nutzer>,<Jobname>,<Dauer in Sekunden>) mit den Typen
     (DateFormat,String,String,Int)

  */

  val data: List[(String,String,String,Int)] =List(("2020.05.08 AD at 10:05:12 CET","meier","wordcount",20),("2020.05.08 AD at 12:05:12 CET","weber","apachelog",50),
    ("2020.05.08 AD at 13:06:17 CET","meier","wordcount",15),("2020.05.08 AD at 15:25:17 CET","meier","wordcount",25),
  ("2020.05.09 AD at 12:00:12 CET","weber","apachelog",45),("2020.05.09 AD at 14:17:22 CET","meier","wordcount",21),
  ("2020.05.09 AD at 18:15:17 CET","weber","apachelog",20),("2020.05.09 AD at 20:05:12 CET","meier","wordcount",17),
  ("2020.05.10 AD at 09:22:33 CET","mueller","MeineAnalyse",100),("2020.05.10 AD at 12:07:02 CET","meier","TF-IDF",2000),
    ("2020.05.10 AD at 12:35:02 CET","mueller","TF-IDF",2100))

  test("Wie viele Jobs haben die einzelnen User abgesetzt?"){

    val exp= Map("meier" -> 6, "weber" -> 3, "mueller" -> 2)
    val res= MapReduce.numberOfJobsPerUser(data)
    assert(res===exp)
  }

  test("Wie häufig haben Benutzer mit Jobs mit einem bestimmten Namen abgesetzt?"){

    val exp= Map(("meier","wordcount") -> 5, ("weber","apachelog") -> 3, ("mueller","MeineAnalyse") -> 1, ("meier","TF-IDF") -> 1,
      ("mueller","TF-IDF") -> 1)
    val res= MapReduce.numberOfJobsPerUserUsingACertainName(data)
    assert(res===exp)
  }

  test("Welche Jobnamen wurden vergeben (ohne Duplikate?"){

    val exp= List("TF-IDF", "MeineAnalyse", "apachelog", "wordcount")
    val res= MapReduce.distinctNamesOfJobs(data)
    assert(res===exp)
  }

  test("Wie viele Aufträge haben über X sec gedauert und wie viele drunter"){

    val exp= Map("less" -> 4, "more" -> 7)
    val res= MapReduce.moreThanXSeconds(data)
    assert(exp===res)
  }

  test("Auftraege pro Tag"){

    val exp= Map("2020-05-08" -> 4, "2020-05-09" -> 4, "2020-05-10" -> 3)
    val res= MapReduce.numberOfJobsPerDay(data)
    assert(res==exp)
  }

}
