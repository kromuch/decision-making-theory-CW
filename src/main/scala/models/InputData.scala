package models

import java.io.File

import scala.io.Source

object InputData {

  private def getListOfFiles(d: File): List[File] = {
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  type RT = Map[Age, Income]

  type UT = Map[Age, Maintenance]

  type CT = Map[Age, RenewCost]

  case class YoITable(rt: RT, ut: UT, ct: CT)

  case class InputDataCC(current: YoITable, other: Map[YoI, YoITable])

  def getInputData(folder: File, currentAge: Int, n: Int): InputDataCC = {
    val yoiOfCurrent = 1 - currentAge
    val table = getListOfFiles(folder).map { file =>
      val yoi: YoI = file.getName.dropRight(4).toInt
      val ages :: incomes :: maintenances :: renews :: _ = Source.fromFile(file).getLines().toList
        .map(_.split("\t").toList.tail.map(_.toInt))
      val rt = ages.zip(incomes).toMap
      val ut = ages.zip(maintenances).toMap
      val ct = ages.zip(renews).toMap
      yoi -> YoITable(rt, ut, ct)
    }.toMap
    InputDataCC(table(yoiOfCurrent), table.filterNot(x => x._1 == yoiOfCurrent || x._1 > n))
  }

}
