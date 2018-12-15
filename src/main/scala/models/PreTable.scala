package models

import models.InputData.InputDataCC

object PreTable {

  /**
    * Одна комірка з двох підкомірок таблиці з етапу умовної оптимізації
    *
    * @param x - міняємо - true, не міняємо - false
    * @param W - прибуток обраного рішення від моменту його прийняття до кінця періоду, що розглядається
    */
  case class PreTableCell(x: Boolean, W: Int, wOther: Int) {
    override def toString: String = s"Замінюємо: ${if (x) "так" else "ні"}, обраний дохід: $W, альтернативний дохід: $wOther"
  }

  type PreTableT = Map[Move, Map[Age, PreTableCell]]

  def generatePreTable(inputData: InputDataCC, currentAge: Int, n: Int): PreTableT = {

    @scala.annotation.tailrec
    def rec(acc: PreTableT, move: Move): PreTableT = {
      if (move == 0)
        acc
      else {
        val preTable: Map[Age, PreTableCell] = if (move == n) {
          val firstMachine: (Age, PreTableCell) = {
            val curr = inputData.current
            val ageOfCurrent = currentAge + move - 1
            /* -1, бо рішення приймається на початку року і на восьмому кроці
                                                               їй буде 10 років, як і на першому кроці 3 роки */
            val toLeave = curr.rt(ageOfCurrent) - curr.ut(ageOfCurrent)
            val toRenew = inputData.other(move).rt(0) - inputData.other(move).ut(0) - curr.ct(ageOfCurrent)
            if (toLeave > toRenew)
              ageOfCurrent -> PreTableCell(x = false, toLeave, toRenew)
            else
              ageOfCurrent -> PreTableCell(x = true, toRenew, toLeave)
          }
          val otherMachines: Map[Age, PreTableCell] = inputData.other.map { case (yoi, yoiTable) =>
            val ageOfThis = move - yoi
            val toLeave = yoiTable.rt(ageOfThis) - yoiTable.ut(ageOfThis)
            val toRenew = inputData.other(move).rt(0) - inputData.other(move).ut(0) - yoiTable.ct(ageOfThis)
            if (toLeave > toRenew)
              ageOfThis -> PreTableCell(x = false, toLeave, toRenew)
            else
              ageOfThis -> PreTableCell(x = true, toRenew, toLeave)
          }
          otherMachines + firstMachine
        } else {
          val firstMachine: (Age, PreTableCell) = {
            val curr = inputData.current
            val ageOfCurrent = currentAge + move - 1
            /* -1, бо рішення приймається на початку року і на восьмому кроці
                                                               їй буде 10 років, як і на першому кроці 3 роки */
            val toLeave = curr.rt(ageOfCurrent) - curr.ut(ageOfCurrent) + acc(move + 1)(ageOfCurrent + 1).W
            val toRenew = inputData.other(move).rt(0) - inputData.other(move).ut(0) - curr.ct(ageOfCurrent) + acc(move + 1)(1).W
            if (toLeave > toRenew)
              ageOfCurrent -> PreTableCell(x = false, toLeave, toRenew)
            else
              ageOfCurrent -> PreTableCell(x = true, toRenew, toLeave)
          }
          val otherMachines: Map[Age, PreTableCell] = inputData.other.flatMap { case (yoi, yoiTable) =>
            val ageOfThis = move - yoi
            if (ageOfThis < 0)
              None
            else Some({
              val toLeave = yoiTable.rt(ageOfThis) - yoiTable.ut(ageOfThis) + acc(move + 1)(ageOfThis + 1).W
              val toRenew = inputData.other(move).rt(0) - inputData.other(move).ut(0) - yoiTable.ct(ageOfThis) + acc(move + 1)(1).W
              if (toLeave > toRenew)
                ageOfThis -> PreTableCell(x = false, toLeave, toRenew)
              else
                ageOfThis -> PreTableCell(x = true, toRenew, toLeave)
            })
          }
          otherMachines + firstMachine
        }
        rec(acc + (move -> preTable), move - 1)
      }
    }

//    (n to 1 by -1).map { move: Move =>
//      val preTable = if (move == n) {
//        val firstMachine: (Age, PreTableCell) = {
//          val curr = inputData.current
//          val ageOfCurrent = currentAge + move - 1
//          /* -1, бо рішення приймається на початку року і на восьмому кроці
//                                                             їй буде 10 років, як і на першому кроці 3 роки */
//          val toLeave = curr.rt(ageOfCurrent) - curr.ut(ageOfCurrent)
//          val toRenew = inputData.other(move).rt(0) - inputData.other(move).ut(0) - curr.ct(ageOfCurrent)
//          if (toLeave > toRenew)
//            ageOfCurrent -> PreTableCell(x = false, toLeave, toRenew)
//          else
//            ageOfCurrent -> PreTableCell(x = true, toRenew, toLeave)
//        }
//        val otherMachines: Map[Age, PreTableCell] = inputData.other.map { case (yoi, yoiTable) =>
//          val ageOfThis = move - yoi
//          val toLeave = yoiTable.rt(ageOfThis) - yoiTable.ut(ageOfThis)
//          val toRenew = inputData.other(move).rt(0) - inputData.other(move).ut(0) - yoiTable.ct(ageOfThis)
//          if (toLeave > toRenew)
//            ageOfThis -> PreTableCell(x = false, toLeave, toRenew)
//          else
//            ageOfThis -> PreTableCell(x = true, toRenew, toLeave)
//        }
//        otherMachines + firstMachine
//      } else Map.empty[Age, PreTableCell]
//      move -> preTable
//    }.toMap

    rec(Map.empty[Move, Map[Age, PreTableCell]], move = n)
  }

}