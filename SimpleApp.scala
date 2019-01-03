/* SimpleApp.scala */
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf



object SimpleApp {
    def main(args: Array[String]) {



        val logFile = "bible+shakes.nopunc" // Should be some file on your system
        val conf = new SparkConf().setAppName("Simple Application")
        val sc = new SparkContext(conf)
      sc.setLogLevel("OFF")
        val logData = sc.textFile(logFile, 2).cache()
        val numAs = logData.filter(line => line.contains("a")).count()
        val numBs = logData.filter(line => line.contains("b")).count()
        println("Lines with a: %s, Lines with b: %s".format(numAs, numBs))

      //split on periods, trim and split on spaces
      //remove special chars, convert to lowercase, and use sliding
      //convert bigram arrays to strings
      //group bigrams and count their frequencies
      val thebigrams =  logData.map{
          _.split('.').map{ _.trim().split(' ').
            map{_.replaceAll("""\W""", "").toLowerCase() }.sliding(2) }
            .flatMap{identity}.map{_.mkString(" ")}.
            groupBy(identity).mapValues(_.size)
        }.flatMap(identity).reduceByKey(_+_)

      println("thebigrams count")
      println(thebigrams.count())

      println("******************************")
      println("******************************")

      println(thebigrams.take(10).toList)

      val asc = thebigrams.sortBy(_._2, false)

      println("******************************")
      println("******************************")
      println("bigrams sorted")
      println(asc.take(10).toList)

      val words = asc.flatMap(_._1.split(" ")).take(10).toList

      val list_of_words = asc.take(1000).toList

      val rankings = for(word <- words) yield list_of_words.filter(x => x._1.contains(word))

        //for(word <- words) yield (asc.filter(x => x._1.contains(word)).takeOrdered(5)(Ordering[Int].on(_._2)) )
      println("******************************")
      println("******************************")
      println("rankings")

      println(rankings.take(100))

    }
}

object FECSQLApp {
  import org.apache.spark.sql.SparkSession

  import org.apache.spark.sql.catalyst.encoders.ExpressionEncoder
  import org.apache.spark.sql.Encoder


  case class CM(CMTE_ID : String, CMTE_NM: String,	TRES_NM: String,
                CMTE_ST1: String, CMTE_ST2: String,	CMTE_CITY: String,
                CMTE_ST: String,	CMTE_ZIP: String,	CMTE_DSGN: String,
                CMTE_TP: String,	CMTE_PTY_AFFILIATION: String,	CMTE_FILING_FREQ: String,
                ORG_TP: String,	CONNECTED_ORG_NM: String,	CAND_ID: String)

  case class CN(CAND_ID: String, 	CAND_NAME: String,	CAND_PTY_AFFILIATION: String,	CAND_ELECTION_YR: Int,
                CAND_OFFICE_ST: String,	CAND_OFFICE: String,	CAND_OFFICE_DISTRICT: String,	CAND_ICI: String,
                CAND_STATUS: String,	CAND_PCC: String,	CAND_ST1: String,	CAND_ST2: String,	CAND_CITY: String,
                CAND_ST: String,	CAND_ZIP: String)

  case class CCL(CAND_ID: String,	CAND_ELECTION_YR: Int,	FEC_ELECTION_YR: Int,
                 CMTE_ID: String,	CMTE_TP: String,	CMTE_DSGN: String,	LINKAGE_ID: String)

  case class ITCONT(CMTE_ID: String, AMNDT_IND: String,	RPT_TP: String,	TRANSACTION_PGI: String,
                    IMAGE_NUM: String,	TRANSACTION_TP: String,	ENTITY_TP: String,	NAME: String,
                    CITY: String,	STATE: String,	ZIP_CODE: String,	EMPLOYER: String,
                    OCCUPATION: String,	TRANSACTION_DT: String,	TRANSACTION_AMT: Float,
                    OTHER_ID: String,	TRAN_ID: String,	FILE_NUM: String,	MEMO_CD: String,
                    MEMO_TEXT: String,	SUB_ID: String)

  case class ITPAS(CMTE_ID: String,AMNDT_IND: String,RPT_TP: String,TRANSACTION_PGI: String,
                   IMAGE_NUM: String,TRANSACTION_TP: String,ENTITY_TP: String, NAME: String,
                   CITY: String,	STATE: String,	ZIP_CODE: String,	EMPLOYER: String,
                   OCCUPATION: String,	TRANSACTION_DT: String,	TRANSACTION_AMT: Float,
                   OTHER_ID: String, CAND_ID: String, TRAN_ID: String, FILE_NUM: String,
                   MEMO_CD: String, MEMO_TEXT: String, SUB_ID: String)

  case class ITOTH(CMTE_ID: String,	AMNDT_IND: String,	RPT_TP: String,	TRANSACTION_PGI: String,
                   IMAGE_NUM: String,	TRANSACTION_TP: String,	ENTITY_TP: String,
                   NAME: String,	CITY: String,	STATE: String,	ZIP_CODE: String,	EMPLOYER: String,
                   OCCUPATION: String,	TRANSACTION_DT: String,	TRANSACTION_AMT: Float,
                   OTHER_ID: String,	TRAN_ID: String,	FILE_NUM: String,	MEMO_CD: String,
                   MEMO_TEXT: String,	SUB_ID: String)

  def main (args: Array[String]): Unit = {

    if (args.size != 1) {
      println("FEC directory path required arg missing!")
      System.exit(1)
    }

    val fecDir = args(0)

    val spark = SparkSession
      .builder()
      .appName("Spark SQL Example")
      .config("spark.some.config.option", "some-value")
      .getOrCreate()

    spark.sparkContext.setLogLevel("OFF")

    // For implicit conversions from RDDs to DataFrames
    import spark.implicits._

    val cmDF = spark.sparkContext
      .textFile(fecDir + "/cm.txt")
      .map(_.split("\\|",-1))
      .map(a => CM(a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9), a(10), a(11), a(12), a(13), a(14)))
      .toDF()

    val cnDF = spark.sparkContext
      .textFile(fecDir + "/cn.txt")
      .map(_.split("\\|",-1))
      .map(a => CN(a(0), a(1), a(2), a(3).trim.toInt, a(4), a(5), a(6), a(7), a(8), a(9), a(10), a(11), a(12), a(13), a(14)))
      .toDF()

    val cclDF = spark.sparkContext
      .textFile(fecDir + "/ccl.txt")
      .map(_.split("\\|",-1))
      .map(a => CCL(a(0), a(1).trim.toInt, a(2).trim.toInt, a(3), a(4), a(5), a(6)))
      .toDF()

    val itcontDF = spark.sparkContext
      .textFile(fecDir + "/itcont.txt")
      .map(_.split("\\|",-1))
      .map(a => ITCONT(a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9), a(10), a(11), a(12), a(13),
                   a(14).trim.toFloat, a(15), a(16), a(17), a(18), a(19), a(20)))
      .toDF()

    val itpasDF = spark.sparkContext
      .textFile(fecDir + "/itpas.txt")
      .map(_.split("\\|",-1))
      .map(a => ITPAS(a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9), a(10), a(11), a(12), a(13),
        a(14).trim.toFloat, a(15), a(16), a(17), a(18), a(19), a(20), a(21)))
      .toDF()

    val itothDF = spark.sparkContext
      .textFile(fecDir + "/itoth.txt")
      .map(_.split("\\|",-1))
      .map(a => ITOTH(a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9), a(10), a(11), a(12), a(13),
        a(14).trim.toFloat, a(15), a(16), a(17), a(18), a(19), a(20)))
      .toDF()

    cmDF.createOrReplaceTempView("cm")
    cnDF.createOrReplaceTempView("cn")
    cclDF.createOrReplaceTempView("ccl")
    itcontDF.createOrReplaceTempView("itcont")
    itpasDF.createOrReplaceTempView("itpas")
    itothDF.createOrReplaceTempView("itothDF")

    //spark.sql("SELECT CAND_ID, CAND_PCC, CAND_NAME FROM cn WHERE CAND_NAME LIKE \"%TRUMP%\" OR CAND_NAME LIKE \"%SANDERS%\" OR CAND_NAME LIKE \"%CLINTON%\" OR CAND_NAME LIKE \"%CRUZ%\"").show()

    val findIDs = cnDF.select('CAND_ID).where('CAND_NAME.contains("TRUMP") && 'CAND_NAME.contains("DONALD J"))

    findIDs.show()

 //   println(findIDs.
  //  println(findIDs.head().toString())
  //  println(findIDs.head().getString(0))

    println("**************************************")

    val t = cclDF.select('CMTE_ID, 'CAND_ID)
    t.show()

    println("**************************************")
    println("**************************************")
    println("**************************************")

    val c2 = cclDF.select('CMTE_ID).filter('CMTE_DSGN === 'P')
    c2.show()

    println("**************************************")
    println("**************************************")
    println("**************************************")

    val c3 = cclDF.select('CMTE_ID).filter('CAND_ID === findIDs.head().getString(0)).filter('CMTE_DSGN === 'P')

    c3.show()

    println("**************************************")
    println("**************************************")
    println("**************************************")
    //H0AK00097
    val c4 = cclDF.select('CMTE_ID).filter('CAND_ID === "H0AK00097")

    c4.show()

    println("**************************************")
    println("**************************************")
    println("**************************************")



    val c5 = cclDF.select('CMTE_ID, 'CMTE_DSGN).filter('CAND_ID === findIDs.head().getString(0))

    c5.show()

    println("**************************************")
    println("**************************************")
    println("**************************************")

    //WARNING DOES NOT WORK WHY THOUGH ??? :(
    val committee_ids = cclDF.select('CMTE_ID).filter(('CAND_ID === findIDs.head().getString(0)) && ('CMTE_DSGN == 'P'))

    committee_ids.show()

    println("**************************************")
    println("**************************************")
    println("LOOK HERE")


    val individual_contributions =  itcontDF.joinWith(c3, itcontDF("CMTE_ID") === committee_ids("CMTE_ID"))

    individual_contributions.show()

    println("the count of contributions for donald trump: " + individual_contributions.count())

    val individual_contributions2 =  itcontDF.select("*").filter('CMTE_ID === c3.head().getString(0))

    individual_contributions2.show()

    println("the count of contributions for donald trump: " + individual_contributions2.count())
//cnDF.

    //get all CMTE_IDs that the CAND_ID is tied to
    //use those to filter the itcontDF
    //count the rows

  }

}
