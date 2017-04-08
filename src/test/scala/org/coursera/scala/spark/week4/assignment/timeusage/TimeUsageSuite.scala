package org.coursera.scala.spark.week4.assignment.timeusage

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{Row, Column}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types.{DoubleType, StringType}
import org.coursera.scala.spark.week4.assignment.timeusage.TimeUsage._
import spark.implicits._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  trait TestSet {

    val testFilePath = "/timeusage/test.csv"
    val assignmentFilePath = "/timeusage/atussum.csv"

    def get[T](rdd: RDD[T], i: Integer): T = {
      rdd.take(i + 1).reverse.head
    }

    val columnNames = List("tucaseid", "gemetsta", "gtmetsta", "peeduca", "pehspnon", "ptdtrace", "teage", "telfs", "temjot",
      "teschenr", "teschlvl", "tesex", "tespempnot", "trchildnum", "trdpftpt", "trernwa", "trholiday", "trspftpt",
      "trsppres", "tryhhchild", "tudiaryday", "tufnwgtp", "tehruslt", "tuyear", "t010101", "t010102", "t010199",
      "t010201", "t010299", "t010301", "t010399", "t010401", "t010499", "t010501", "t010599", "t019999", "t020101",
      "t020102", "t020103", "t020104", "t020199", "t020201", "t020202", "t020203", "t020299", "t020301", "t020302",
      "t020303", "t020399", "t020401", "t020402", "t020499", "t020501", "t020502", "t020599", "t020681", "t020699",
      "t020701", "t020799", "t020801", "t020899", "t020901", "t020902", "t020903", "t020904", "t020905", "t020999",
      "t029999", "t030101", "t030102", "t030103", "t030104", "t030105", "t030108", "t030109", "t030110", "t030111",
      "t030112", "t030186", "t030199", "t030201", "t030202", "t030203", "t030204", "t030299", "t030301", "t030302",
      "t030303", "t030399", "t030401", "t030402", "t030403", "t030404", "t030405", "t030499", "t030501", "t030502",
      "t030503", "t030504", "t030599", "t039999", "t040101", "t040102", "t040103", "t040104", "t040105", "t040108",
      "t040109", "t040110", "t040111", "t040112", "t040186", "t040199", "t040201", "t040202", "t040203", "t040204",
      "t040299", "t040301", "t040302", "t040303", "t040399", "t040401", "t040402", "t040403", "t040404", "t040405",
      "t040499", "t040501", "t040502", "t040503", "t040504", "t040505", "t040506", "t040507", "t040508", "t040599",
      "t049999", "t050101", "t050102", "t050103", "t050189", "t050201", "t050202", "t050203", "t050204", "t050289",
      "t050301", "t050302", "t050303", "t050304", "t050389", "t050403", "t050404", "t050405", "t050481", "t050499",
      "t059999", "t060101", "t060102", "t060103", "t060104", "t060199", "t060201", "t060202", "t060203", "t060289",
      "t060301", "t060302", "t060303", "t060399", "t060401", "t060402", "t060403", "t060499", "t069999", "t070101",
      "t070102", "t070103", "t070104", "t070105", "t070199", "t070201", "t070299", "t070301", "t070399", "t079999",
      "t080101", "t080102", "t080199", "t080201", "t080202", "t080203", "t080299", "t080301", "t080302", "t080399",
      "t080401", "t080402", "t080403", "t080499", "t080501", "t080502", "t080599", "t080601", "t080602", "t080699",
      "t080701", "t080702", "t080799", "t080801", "t080899", "t089999", "t090101", "t090102", "t090103", "t090104",
      "t090199", "t090201", "t090202", "t090299", "t090301", "t090302", "t090399", "t090401", "t090402", "t090499",
      "t090501", "t090502", "t090599", "t099999", "t100101", "t100102", "t100103", "t100199", "t100201", "t100299",
      "t100381", "t100383", "t100399", "t100401", "t100499", "t109999", "t110101", "t110199", "t110281", "t110289",
      "t119999", "t120101", "t120199", "t120201", "t120202", "t120299", "t120301", "t120302", "t120303", "t120304",
      "t120305", "t120306", "t120307", "t120308", "t120309", "t120310", "t120311", "t120312", "t120313", "t120399",
      "t120401", "t120402", "t120403", "t120404", "t120405", "t120499", "t120501", "t120502", "t120503", "t120504",
      "t120599", "t129999", "t130101", "t130102", "t130103", "t130104", "t130105", "t130106", "t130107", "t130108",
      "t130109", "t130110", "t130111", "t130112", "t130113", "t130114", "t130115", "t130116", "t130117", "t130118",
      "t130119", "t130120", "t130121", "t130122", "t130123", "t130124", "t130125", "t130126", "t130127", "t130128",
      "t130129", "t130130", "t130131", "t130132", "t130133", "t130134", "t130135", "t130136", "t130199", "t130201",
      "t130202", "t130203", "t130204", "t130205", "t130206", "t130207", "t130208", "t130209", "t130210", "t130211",
      "t130212", "t130213", "t130214", "t130215", "t130216", "t130217", "t130218", "t130219", "t130220", "t130221",
      "t130222", "t130223", "t130224", "t130225", "t130226", "t130227", "t130228", "t130229", "t130230", "t130231",
      "t130232", "t130299", "t130301", "t130302", "t130399", "t130401", "t130402", "t130499", "t139999", "t140101",
      "t140102", "t140103", "t140104", "t140105", "t149999", "t150101", "t150102", "t150103", "t150104", "t150105",
      "t150106", "t150199", "t150201", "t150202", "t150203", "t150204", "t150299", "t150301", "t150302", "t150399",
      "t150401", "t150402", "t150499", "t150501", "t150599", "t150601", "t150602", "t150699", "t159989", "t160101",
      "t160102", "t160103", "t160104", "t160105", "t160106", "t160107", "t160108", "t169989", "t180101", "t180199",
      "t180280", "t180381", "t180382", "t180399", "t180481", "t180482", "t180499", "t180501", "t180502", "t180589",
      "t180601", "t180682", "t180699", "t180701", "t180782", "t180801", "t180802", "t180803", "t180804", "t180805",
      "t180806", "t180807", "t180899", "t180901", "t180902", "t180903", "t180904", "t180905", "t180999", "t181002",
      "t181081", "t181099", "t181101", "t181199", "t181201", "t181202", "t181204", "t181283", "t181299", "t181301",
      "t181302", "t181399", "t181401", "t181499", "t181501", "t181599", "t181601", "t181699", "t181801", "t181899",
      "t189999", "t500101", "t500103", "t500104", "t500105", "t500106", "t500107", "t509989")
  }

  override def afterAll(): Unit = {
    spark.close()
  }

  test("dfSchema getting columns") {
    new TestSet {
      def schema = dfSchema(columnNames)

      assert(schema.fieldNames === columnNames.toArray)
      assert(schema.fields.head.dataType === StringType)
      assert(schema.fields.head.nullable === false)
      assert(schema.fields.tail.forall(field => !field.nullable && field.dataType == DoubleType))
    }
  }

  test("fsPath for getting file") {
    new TestSet {
      assert(fsPath(testFilePath).endsWith("test.csv"))
      assert(fsPath(assignmentFilePath).endsWith("atussum.csv"))
      intercept[NullPointerException] {
        fsPath("/timeusage/blahblah.csv")
      }
    }
  }

  test("row for transforming line to proper case class") {
    new TestSet {
      val rdd = spark.sparkContext.textFile(fsPath(testFilePath))
      val result = row(get(rdd, 1).split(",").toList)
      assert(columnNames.length === result.length)
      assert("20030100013280" === result.getString(0))
      assert(1 === result.getDouble(1))
      assert(-1 === result.getDouble(2))
      assert(44 === result.getDouble(3))
      assert(2 === result.getDouble(4))
    }
  }

  test("read for getting file") {
    new TestSet {
      val (columns, initDf) = read(testFilePath)
      assert(columnNames === columns)
      val result = initDf.first()
      assert(columnNames.length === result.length)
      assert("20030100013280" === result.getString(0))
      assert(1 === result.getDouble(1))
      assert(-1 === result.getDouble(2))
      assert(44 === result.getDouble(3))
      assert(2 === result.getDouble(4))
    }
  }

  test("classifiedColumns for getting interesting columns") {
    new TestSet {
      val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columnNames)

      //"t01.*", "t03.*", "t11.*", "t1801.*", "t1803.*"
      val primaryNeedsColumnNames = List(
        "t010101", "t010102", "t010199", "t010201", "t010299", "t010301", "t010399", "t010401", "t010499", "t010501", "t010599", "t019999",
        "t030101", "t030102", "t030103", "t030104", "t030105", "t030108", "t030109", "t030110", "t030111", "t030112", "t030186", "t030199",
        "t030201", "t030202", "t030203", "t030204", "t030299", "t030301", "t030302", "t030303", "t030399", "t030401", "t030402", "t030403",
        "t030404", "t030405", "t030499", "t030501", "t030502", "t030503", "t030504", "t030599", "t039999",
        "t110101", "t110199", "t110281", "t110289", "t119999",
        "t180101", "t180199",
        "t180381", "t180382", "t180399"
      )
      //"t05.*", "t1805.*"
      val workColumnNames = List(
        "t050101", "t050102", "t050103", "t050189", "t050201", "t050202", "t050203", "t050204", "t050289", "t050301", "t050302", "t050303",
        "t050304", "t050389", "t050403", "t050404", "t050405", "t050481", "t050499", "t059999",
        "t180501", "t180502", "t180589"
      )

      //"t02.*", "t04.*", "t06.*", “t07.*”, “t08.*”, “t09.*”, "t10.*", "t12.*", "t13.*", "t14.*", "t15.*", "t16.*", “t18.*” and not "t1801.*", "t1803.*", "t1805.*"
      val otherColumnNames = List(
        "t020101", "t020102", "t020103", "t020104", "t020199", "t020201", "t020202", "t020203", "t020299", "t020301", "t020302", "t020303",
        "t020399", "t020401", "t020402", "t020499", "t020501", "t020502", "t020599", "t020681", "t020699", "t020701", "t020799", "t020801",
        "t020899", "t020901", "t020902", "t020903", "t020904", "t020905", "t020999", "t029999",
        "t040101", "t040102", "t040103", "t040104", "t040105", "t040108", "t040109", "t040110", "t040111", "t040112", "t040186", "t040199",
        "t040201", "t040202", "t040203", "t040204", "t040299", "t040301", "t040302", "t040303", "t040399", "t040401", "t040402", "t040403",
        "t040404", "t040405", "t040499", "t040501", "t040502", "t040503", "t040504", "t040505", "t040506", "t040507", "t040508", "t040599",
        "t049999",
        "t060101", "t060102", "t060103", "t060104", "t060199", "t060201", "t060202", "t060203", "t060289", "t060301", "t060302", "t060303",
        "t060399", "t060401", "t060402", "t060403", "t060499", "t069999",
        "t070101", "t070102", "t070103", "t070104", "t070105", "t070199", "t070201", "t070299", "t070301", "t070399", "t079999",
        "t080101", "t080102", "t080199", "t080201", "t080202", "t080203", "t080299", "t080301", "t080302", "t080399", "t080401", "t080402",
        "t080403", "t080499", "t080501", "t080502", "t080599", "t080601", "t080602", "t080699", "t080701", "t080702", "t080799", "t080801",
        "t080899", "t089999",
        "t090101", "t090102", "t090103", "t090104", "t090199", "t090201", "t090202", "t090299", "t090301", "t090302", "t090399", "t090401",
        "t090402", "t090499", "t090501", "t090502", "t090599", "t099999",
        "t100101", "t100102", "t100103", "t100199", "t100201", "t100299", "t100381", "t100383", "t100399", "t100401", "t100499", "t109999",
        "t120101", "t120199", "t120201", "t120202", "t120299", "t120301", "t120302", "t120303", "t120304", "t120305", "t120306", "t120307",
        "t120308", "t120309", "t120310", "t120311", "t120312", "t120313", "t120399", "t120401", "t120402", "t120403", "t120404", "t120405",
        "t120499", "t120501", "t120502", "t120503", "t120504", "t120599", "t129999",
        "t130101", "t130102", "t130103", "t130104", "t130105", "t130106", "t130107", "t130108", "t130109", "t130110", "t130111", "t130112",
        "t130113", "t130114", "t130115", "t130116", "t130117", "t130118", "t130119", "t130120", "t130121", "t130122", "t130123", "t130124",
        "t130125", "t130126", "t130127", "t130128", "t130129", "t130130", "t130131", "t130132", "t130133", "t130134", "t130135", "t130136",
        "t130199", "t130201", "t130202", "t130203", "t130204", "t130205", "t130206", "t130207", "t130208", "t130209", "t130210", "t130211",
        "t130212", "t130213", "t130214", "t130215", "t130216", "t130217", "t130218", "t130219", "t130220", "t130221", "t130222", "t130223",
        "t130224", "t130225", "t130226", "t130227", "t130228", "t130229", "t130230", "t130231", "t130232", "t130299", "t130301", "t130302",
        "t130399", "t130401", "t130402", "t130499", "t139999",
        "t140101", "t140102", "t140103", "t140104", "t140105", "t149999",
        "t150101", "t150102", "t150103", "t150104", "t150105", "t150106", "t150199", "t150201", "t150202", "t150203", "t150204", "t150299",
        "t150301", "t150302", "t150399", "t150401", "t150402", "t150499", "t150501", "t150599", "t150601", "t150602", "t150699", "t159989",
        "t160101", "t160102", "t160103", "t160104", "t160105", "t160106", "t160107", "t160108", "t169989",
        "t180280", "t180481", "t180482", "t180499", "t180601", "t180682", "t180699", "t180701", "t180782", "t180801", "t180802", "t180803",
        "t180804", "t180805", "t180806", "t180807", "t180899", "t180901", "t180902", "t180903", "t180904", "t180905", "t180999", "t181002",
        "t181081", "t181099", "t181101", "t181199", "t181201", "t181202", "t181204", "t181283", "t181299", "t181301", "t181302", "t181399",
        "t181401", "t181499", "t181501", "t181599", "t181601", "t181699", "t181801", "t181899", "t189999"
      )

      assert(primaryNeedsColumnNames.map(new Column(_)) === primaryNeedsColumns)
      assert(workColumnNames.map(new Column(_)) === workColumns)
      assert(otherColumnNames.map(new Column(_)) === otherColumns)
    }
  }

  test("timeUsageSummary for getting interesting data about time usages") {
    new TestSet {
      val (_, dataFrame) = read(testFilePath)
      val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columnNames)
      val timeUsageFrame = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, dataFrame)

      val result = timeUsageFrame.collect()

      assert(Array("working", "sex", "age", "primaryNeeds", "work", "other") === timeUsageFrame.columns)
      assert(3 === result.length)
      assert("working" === result(0).getString(0))
      assert("male" === result(0).getString(1))
      assert("elder" === result(0).getString(2))
      assert(15.25 === result(0).getDouble(3))
      assert(0.0 === result(0).getDouble(4))
      assert(8.75 === result(0).getDouble(5))

      assert("working" === result(1).getString(0))
      assert("female" === result(1).getString(1))
      assert("active" === result(1).getString(2))
      assert(13.833333333333334 === result(1).getDouble(3))
      assert(0.0 === result(1).getDouble(4))
      assert(10.166666666666666 === result(1).getDouble(5))

      assert("not working" === result(2).getString(0))
      assert("female" === result(2).getString(1))
      assert("young" === result(2).getString(2))
      assert(11.916666666666666 === result(2).getDouble(3))
      assert(0.0 === result(2).getDouble(4))
      assert(12.083333333333334 === result(2).getDouble(5))
    }
  }

  test("timeUsageGrouped for getting grouped statistics") {
    new TestSet {
      val (columns, dataFrame) = read(testFilePath)
      val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
      val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, dataFrame)
      val finalDf = timeUsageGrouped(summaryDf)

      val result = finalDf.collect()

      assert(Array("working", "sex", "age", "primaryNeeds", "work", "other") === finalDf.columns)
      assert(3 === result.length)
      assert("not working" === result(0).getString(0))
      assert("female" === result(0).getString(1))
      assert("young" === result(0).getString(2))
      assert(11.9 === result(0).getDouble(3))
      assert(0.0 === result(0).getDouble(4))
      assert(12.1 === result(0).getDouble(5))

      assert("working" === result(1).getString(0))
      assert("female" === result(1).getString(1))
      assert("active" === result(1).getString(2))
      assert(13.8 === result(1).getDouble(3))
      assert(0.0 === result(1).getDouble(4))
      assert(10.2 === result(1).getDouble(5))

      assert("working" === result(2).getString(0))
      assert("male" === result(2).getString(1))
      assert("elder" === result(2).getString(2))
      assert(15.3 === result(2).getDouble(3))
      assert(0.0 === result(2).getDouble(4))
      assert(8.8 === result(2).getDouble(5))
    }
  }

  ignore("timeUsageGrouped for getting grouped statistics - all data") {
    new TestSet {
      val (columns, dataFrame) = read(assignmentFilePath)
      val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
      val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, dataFrame)
      val finalDf = timeUsageGrouped(summaryDf)

      val result = finalDf.collect()

      assert(Array("working", "sex", "age", "primaryNeeds", "work", "other") === finalDf.columns)
      assert(12 === result.length)

      def getRow(result: Array[Row], index: Integer) = (result(index).getString(0), result(index).getString(1), result(index).getString(2), result(index).getDouble(3), result(index).getDouble(4), result(index).getDouble(5))
      assert(("not working", "female", "active", 12.4, 0.5, 10.8) === getRow(result, 0))
      assert(("not working", "female", "elder", 10.9,  0.4,  12.4) === getRow(result, 1))
      assert(("not working", "female", "young", 12.5,  0.2,  11.1) === getRow(result, 2))
      assert(("not working", "male", "active", 11.4,  0.9,  11.4) === getRow(result, 3))
      assert(("not working", "male", "elder", 10.7,  0.7,  12.3) === getRow(result, 4))
      assert(("not working", "male", "young", 11.6, 0.2,  11.9) === getRow(result, 5))
      assert(("working", "female", "active", 11.5, 4.2, 8.1) === getRow(result, 6))
      assert(("working", "female", "elder", 10.6, 3.9, 9.3) === getRow(result, 7))
      assert(("working", "female", "young", 11.6, 3.3, 8.9) === getRow(result, 8))
      assert(("working", "male", "active", 10.8, 5.2, 7.8) === getRow(result, 9))
      assert(("working", "male", "elder", 10.4, 4.8, 8.6) === getRow(result, 10))
      assert(("working", "male", "young", 10.9, 3.7, 9.2) === getRow(result, 11))
    }
  }

  test("timeUsageGroupedSql for getting grouped statistics") {
    new TestSet {
      val (columns, dataFrame) = read(testFilePath)
      val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
      val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, dataFrame)
      val finalDf = timeUsageGroupedSql(summaryDf)

      val result = finalDf.collect()

      assert(Array("working", "sex", "age", "primaryNeeds", "work", "other") === finalDf.columns)
      assert(3 === result.length)
      assert("not working" === result(0).getString(0))
      assert("female" === result(0).getString(1))
      assert("young" === result(0).getString(2))
      assert(11.9 === result(0).getDouble(3))
      assert(0.0 === result(0).getDouble(4))
      assert(12.1 === result(0).getDouble(5))

      assert("working" === result(1).getString(0))
      assert("female" === result(1).getString(1))
      assert("active" === result(1).getString(2))
      assert(13.8 === result(1).getDouble(3))
      assert(0.0 === result(1).getDouble(4))
      assert(10.2 === result(1).getDouble(5))

      assert("working" === result(2).getString(0))
      assert("male" === result(2).getString(1))
      assert("elder" === result(2).getString(2))
      assert(15.3 === result(2).getDouble(3))
      assert(0.0 === result(2).getDouble(4))
      assert(8.8 === result(2).getDouble(5))
    }
  }

  test("timeUsageSummaryTyped for getting interesting data about time usages") {
    new TestSet {
      val (_, dataFrame) = read(testFilePath)
      val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columnNames)
      val timeUsageFrame = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, dataFrame)
      val timeUsageDataSet = timeUsageSummaryTyped(timeUsageFrame)

      timeUsageDataSet.show()
      val result = timeUsageDataSet.collect()

      assert(Array("working", "sex", "age", "primaryNeeds", "work", "other") === timeUsageDataSet.columns)
      assert(3 === result.length)
      assert("working" === result(0).working)
      assert("male" === result(0).sex)
      assert("elder" === result(0).age)
      assert(15.25 === result(0).primaryNeeds)
      assert(0.0 === result(0).work)
      assert(8.75 === result(0).other)

      assert("working" === result(1).working)
      assert("female" === result(1).sex)
      assert("active" === result(1).age)
      assert(13.833333333333334 === result(1).primaryNeeds)
      assert(0.0 === result(1).work)
      assert(10.166666666666666 === result(1).other)

      assert("not working" === result(2).working)
      assert("female" === result(2).sex)
      assert("young" === result(2).age)
      assert(11.916666666666666 === result(2).primaryNeeds)
      assert(0.0 === result(2).work)
      assert(12.083333333333334 === result(2).other)
    }
  }

}
