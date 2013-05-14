package actors

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.util.Timeout
import models.ScalarProductItem

class ScalarProductCalculatorMaster(vector1: Array[Int], vector2: Array[Int]) extends Actor {

  private implicit val timeout = Timeout(5000)
  private implicit val system = ActorSystem("ActorSystem")
  private var permutations = vector2.permutations.toList

  def receive = {
    case GetResultMessage =>
      println("MASTER: asked for the result")
      val listOfFutures: List[scala.concurrent.Future[ResultMessage]] =
        permutations.map(
          { (p) =>
            val actor = context.actorOf(Props(new ScalarProductCalculatorWorker()))
            akka.pattern.ask(actor, CalculateMessage(ScalarProductItem(vector1, p, 0))).mapTo[ResultMessage]
          })
      val futureList: Future[List[ResultMessage]] = Future.sequence(listOfFutures)
      val min = Await.result(futureList, timeout.duration).asInstanceOf[List[ResultMessage]].minBy(_.item.scalarProduct)
      println("MASTER: returning min = " + min.item.scalarProduct)
      sender ! min.item
      context.stop(self)
  }
}