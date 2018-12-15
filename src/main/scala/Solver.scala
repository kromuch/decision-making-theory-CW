import models.{Age, Move}
import models.PreTable.PreTableT

object Solver {

  @scala.annotation.tailrec
  def solve(preTable: PreTableT, t: Age, move: Move, n: Int): Unit = {
    if (move > n)
      ()
    else {
      if (preTable(move)(t).x) {
        println(s"На кроці $move була машина віку $t, її замінили")
        solve(preTable, t = 1, move + 1, n)
      } else {
        println(s"На кроці $move була машина віку $t, її продовжили використовувати")
        solve(preTable, t = t + 1, move + 1, n)
      }
    }
  }

}
