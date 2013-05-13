package controllers

import actors._
import akka._
import akka.actor._
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import models._
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.iteratee.Enumerator
import play.api.mvc._
import scala.concurrent.Await
import scala.collection.mutable.ArrayBuffer

object MinimumScalarProductController extends Controller {

  private implicit val system = ActorSystem("ActorSystem")
  private implicit val timeout = Timeout(5000)

  def minimumScalarProductSync(input1: String, input2: String) = {
    val r = minimumScalarProductResult(input1, input2)
    renderResult(r)
  }

  def minimumScalarProductAsync(input1: String, input2: String) = Action { implicit request =>
    val futureResult: scala.concurrent.Future[ScalarProductResult] =
      scala.concurrent.Future[ScalarProductResult] {
        minimumScalarProductResult(input1, input2)
      }
    val timeoutFuture = play.api.libs.concurrent.Promise.timeout("Oops, timeout", 5000)

    Async {
      // We are calling a method in the companion object of Future trait.
      scala.concurrent.Future.firstCompletedOf(Seq(futureResult, timeoutFuture)).map {
        case r: ScalarProductResult =>
          render {
            case Accepts.Html() => Ok(views.html.minimumScalarProduct(r))
            case Accepts.Json() => Ok(r.toJson)
          }
        case t: String => InternalServerError(t)
      }
    }
  }

  def minimumScalarProductUsingOneActor(input1: String, input2: String) = {
    val master = system.actorOf(Props(new Actor() {
      def receive = {
        case GetResultMessage =>
          sender ! minimumScalarProductResult(input1, input2)
      }
    }), name = "master")
    val future = master ? GetResultMessage
    val result = Await.result(future, timeout.duration).asInstanceOf[ScalarProductResult]
    system.stop(master)
    renderResult(result)
  }

  def minimumScalarProductUsingActors(input1: String, input2: String) = {
    implicit val timeout = Timeout(5000)
    println
    println
    println
    val vector1 = string2vector(input1)
    val vector2 = string2vector(input2)
    val master = system.actorOf(Props(new ScalarProductCalculatorMaster(vector1, vector2)))
    val future = master ? GetResultMessage
    val futureResult = Await.result(future, timeout.duration).asInstanceOf[ScalarProductItem]
    val firstScalarProduct = scalarProduct(vector1, vector2) 
    val result = ScalarProductResult(input1, input2, firstScalarProduct, futureResult);
    println("CONTROLLER: Receiving minimum = " + futureResult.scalarProduct)
    renderResult(result)
  }

  // PRIVATE METHODS
  
  private def string2vector(input: String) = input.split(",").map(_.toInt)
  
  private def minimumScalarProductResult(input1: String, input2: String): ScalarProductResult = {
    val vector1 = string2vector(input1)
    val vector2 = string2vector(input2)

    /*val result = input1.split(",").map(_.toInt)
      .zip(input2.split(",").map(_.toInt))
      .foldLeft(0) { (total, n) => total + n._1 * n._2 }*/
    val result = scalarProduct(vector1, vector2)

    val minScalarProduct = vector1.permutations
      .map({ (p) => ScalarProductItem(p, vector2, scalarProduct(p, vector2)) })
      .foldLeft(new ArrayBuffer[ScalarProductItem]) { (permutationAndScalarProduct2, p) => permutationAndScalarProduct2 += p }.minBy(_.scalarProduct)
    ScalarProductResult(input1, input2, result, minScalarProduct)
  }

  private def renderResult(result: ScalarProductResult) = Action {
    implicit request =>
      render {
        case Accepts.Html() => Ok(views.html.minimumScalarProduct(result))
        case Accepts.Json() => Ok(result.toJson)
      }
  }
  
  private def scalarProduct(vector1: Array[Int], vector2: Array[Int]): Int = {
    vector1
      .zip(vector2)
      .foldLeft(0) { (total, n) => total + n._1 * n._2 }
  }
}