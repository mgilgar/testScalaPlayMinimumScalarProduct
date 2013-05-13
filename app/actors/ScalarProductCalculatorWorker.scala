package actors

import akka.actor._
import controllers._
import models._

class ScalarProductCalculatorWorker extends Actor {

  def receive = {
    case CalculateMessage(item) =>
      sender ! new ResultMessage(new ScalarProductItem(item.vector1, item.vector2, scalarProduct(item.vector1, item.vector2)))
      context.stop(self)
  }

  private def scalarProduct(vector1: Array[Int], vector2: Array[Int]): Int = {
    vector1
      .zip(vector2)
      .foldLeft(0) { (total, n) => total + n._1 * n._2 }
  }
}