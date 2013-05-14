package actors

import akka.actor._
import controllers._
import models._
import services._

class ScalarProductCalculatorWorker extends Actor {

  def receive = {
    case CalculateMessage(item) =>
      sender ! new ResultMessage(new ScalarProductItem(item.vector1, item.vector2, MinimumScalarProductService.scalarProduct(item.vector1, item.vector2)))
      context.stop(self)
  }
}