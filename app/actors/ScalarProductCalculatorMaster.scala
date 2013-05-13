package actors

import actors._
import akka._
import akka.actor._
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import controllers._
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.CountDownLatch
import models._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent._
import ExecutionContext.Implicits.global

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