package wordcount

import java.awt.{Color, GridLayout}

import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYDotRenderer
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.ui.ApplicationFrame
import org.jfree.util.ShapeUtilities


/**
  * @author hendrik
  * modified by akarakochev
  */
class Sentiments(sentiFile: String) {

  val sentiments: Map[String, Int] = getSentiments(sentiFile)

  val proc = new Processing()

  /** ********************************************************************************************
    *
    * Aufgabe 3
    *
    * ********************************************************************************************
    */

  def getDocumentGroupedByCounts(filename: String, wordCount: Int): List[(Int, List[String])] = {
    val words = proc.getAllWords(Processing.getData(filename))


    def countHelper(index: Int, oldList: List[String], newList: List[String], list: List[(Int, List[String])] ): List[(Int, List[String])] = {
      if(oldList.isEmpty){
        list.appended(index, newList)
      }
      else if (newList.length < wordCount){
        countHelper(index, oldList.tail, newList.appended(oldList.head), list)
      }
      else {
        countHelper(index+1, oldList, List.empty, list.appended(index, newList) )
      }
    }


    countHelper(1, words, List.empty, List.empty)

  }
  def getDocumentSplitByPredicate(filename: String, predicate:String=>Boolean): List[(Int, List[String])] = {
    val words = proc.getAllWords(Processing.getData(filename))
    //list.take ist nützlich


    def countHelper(index: Int, oldList: List[String], newList: List[String], list: List[(Int, List[String])] ): List[(Int, List[String])] = {
      if(oldList.isEmpty){
        list.appended(index, newList)
      }
      else if (predicate(oldList.head)){
        if(index == 0) countHelper(index+1, oldList.tail, List.empty, list)
        else countHelper(index+1, oldList.tail, List.empty, list.appended(index, newList))
      }
      else {
        countHelper(index, oldList.tail, newList.appended(oldList.head), list)
      }
    }
    countHelper(0, words, List.empty, List.empty)
  }

  def analyseSentiments(l: List[(Int, List[String])]): List[(Int, Double, Double)] = {

    val helper = for(x <- l) yield (x._1, (for(y <- x._2 if(sentiments.contains(y))) yield(sentiments(y))), x._2.length)

    for(x <- helper) yield (x._1, (x._2.foldLeft(0)((a,b) => (a+b))).toFloat/x._2.length, (x._2.length).toFloat/x._3)

  }

  /** ********************************************************************************************
    *
    * Helper Functions
    *
    * ********************************************************************************************
    */

  def getSentiments(filename: String): Map[String, Int] = {
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url.replaceAll("%20"," "))
    val iter = src.getLines()
    val result: Map[String, Int] = (for (row <- iter) yield {
      val seg = row.split("\t"); (seg(0) -> seg(1).toInt)
    }).toMap
    src.close()
    result
  }

  def createGraph(data: List[(Int, Double, Double)], xlabel:String="Abschnitt", title:String="Sentiment-Analyse"): Unit = {

    //create xy series
    val sentimentsSeries: XYSeries = new XYSeries("Sentiment-Werte")
    data.foreach { case (i, sentimentValue, _) => sentimentsSeries.add(i, sentimentValue) }
    val relWordsSeries: XYSeries = new XYSeries("Relative Haeufigkeit der erkannten Worte")
    data.foreach { case (i, _, relWordsValue) => relWordsSeries.add(i, relWordsValue) }

    //create xy collections
    val sentimentsDataset: XYSeriesCollection = new XYSeriesCollection()
    sentimentsDataset.addSeries(sentimentsSeries)
    val relWordsDataset: XYSeriesCollection = new XYSeriesCollection()
    relWordsDataset.addSeries(relWordsSeries)

    //create renderers
    val relWordsDot: XYDotRenderer = new XYDotRenderer()
    relWordsDot.setDotHeight(5)
    relWordsDot.setDotWidth(5)
    relWordsDot.setSeriesShape(0, ShapeUtilities.createDiagonalCross(3, 1))
    relWordsDot.setSeriesPaint(0, Color.BLUE)

    val sentimentsDot: XYDotRenderer = new XYDotRenderer()
    sentimentsDot.setDotHeight(5)
    sentimentsDot.setDotWidth(5)

    //create xy axis
    val xax: NumberAxis = new NumberAxis(xlabel)
    val y1ax: NumberAxis = new NumberAxis("Sentiment Werte")
    val y2ax: NumberAxis = new NumberAxis("Relative Haeufigfkeit")

    //create plots
    val plot1: XYPlot = new XYPlot(sentimentsDataset, xax, y1ax, sentimentsDot)
    val plot2: XYPlot = new XYPlot(relWordsDataset, xax, y2ax, relWordsDot)

    val chart1: JFreeChart = new JFreeChart(plot1)
    val chart2: JFreeChart = new JFreeChart(plot2)
    val frame: ApplicationFrame = new ApplicationFrame(title)
    frame.setLayout(new GridLayout(2,1))

    val chartPanel1: ChartPanel = new ChartPanel(chart1)
    val chartPanel2: ChartPanel = new ChartPanel(chart2)

    frame.add(chartPanel1)
    frame.add(chartPanel2)
    frame.pack()
    frame.setVisible(true)
    
    println("Please press enter....")
    System.in.read()
    frame.setVisible(false)
    frame.dispose
  }
}
