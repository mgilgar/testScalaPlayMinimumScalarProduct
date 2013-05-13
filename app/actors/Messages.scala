package actors

import models._

sealed trait ScalarProductMessage

case class CalculateMessage(item: ScalarProductItem) extends ScalarProductMessage
case class GetResultMessage() extends ScalarProductMessage
case class ResultMessage(item: ScalarProductItem) extends ScalarProductMessage
