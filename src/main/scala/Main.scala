import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import models.{InputData, PreTable}

object Main {

  val config: Config = ConfigFactory.load()

  val n: Int = config.getInt("n")
  val currentAge: Int = config.getInt("currentAge")

  def main(args: Array[String]): Unit = {
    val inputData = InputData.getInputData(new File("input/master/"), currentAge, n)
    val preTable = PreTable.generatePreTable(inputData, currentAge, n)

    println("==========Етап умовної оптимізації==========\n")
    preTable.toList.sortBy(-_._1).foreach { case (move, map) =>
      println(s"Крок: $move")
        map.toList.sortBy(_._1).foreach { case (age, cell) =>
            println(s"$age\t$cell")
        }
    }

    println("\n\n\n==========Етап безумовної оптимізації==========\n")

    Solver.solve(preTable, 3, 1, n)


  }
}