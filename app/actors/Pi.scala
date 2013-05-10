package actors

import akka.actor._
import akka.routing.RoundRobinRouter
import controllers._

sealed trait ScalarProductMessage

//case class Calculate(item: ScalarProductItem, masterActor: ScalarProductCalculatorMaster) extends ScalarProductMessage
case class Calculate(item: ScalarProductItem, masterActor: ActorRef) extends ScalarProductMessage
//case class Result(item: ScalarProductItem, workerActor: ActorRef) extends ScalarProductMessage
case class GetResult()
case class ResultMessage(item: ScalarProductItem )
