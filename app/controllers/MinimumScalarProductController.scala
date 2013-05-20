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
import play.api.libs._
import play.api.libs.json._
import scala.concurrent.Await
import scala.collection.mutable.ArrayBuffer
import services._
import scala.concurrent.TimeoutException

object MinimumScalarProductController extends Controller {

  private implicit val system = ActorSystem("ActorSystem")
  private implicit val timeout = Timeout(1000)

  def minimumScalarProductSync(input1: String, input2: String) = {
    renderResult(MinimumScalarProductService.minimumScalarProductResult(string2array(input1), string2array(input2)))
  }

  def minimumScalarProductAsync(input1: String, input2: String) = Action { implicit request =>
    val futureResult: scala.concurrent.Future[ScalarProductResult] =
      scala.concurrent.Future[ScalarProductResult] {
        MinimumScalarProductService.minimumScalarProductResult(string2array(input1), string2array(input2))
      }
    val timeoutFuture = play.api.libs.concurrent.Promise.timeout("Oops, timeout", timeout.duration)

    Async {
      // We are calling a method in the companion object of scala.concurrent.Future trait.
      // TODO: Not sure what is the different between this way of doing things and Await.result
      // TODO: Not sure if the standard now is akka future or scala future
      scala.concurrent.Future.firstCompletedOf(Seq(futureResult, timeoutFuture)).map {
        case r: ScalarProductResult =>
          // TODO: I have to find a away to improve this, I should be using renderResult
          render {
            case Accepts.Html() => Ok(views.html.minimumScalarProduct(r))
            case Accepts.Json() => Ok(Json.toJson(r))
          }
        case t: String => InternalServerError(t)
      }
    }
  }

  def minimumScalarProductUsingOneActor(input1: String, input2: String) = {
    val master = system.actorOf(Props(new Actor() {
      def receive = {
        case GetResultMessage =>
          sender ! MinimumScalarProductService.minimumScalarProductResult(string2array(input1), string2array(input2))
          context.stop(self)
      }
    }), name = "master")
    val future = master ? GetResultMessage
    try {
    	val result = Await.result(future, timeout.duration).asInstanceOf[ScalarProductResult]    	
    	renderResult(result)
    } catch {
      	case t: TimeoutException => Action { implicit request => InternalServerError(t.getMessage()) }
    }
  }

  def minimumScalarProductUsingActors(input1: String, input2: String) = {
    val vector1 = string2array(input1)
    val vector2 = string2array(input2)
    val master = system.actorOf(Props(new ScalarProductCalculatorMaster(vector1, vector2)))
    val future = master ? GetResultMessage
    try {
    	val futureResult = Await.result(future, timeout.duration).asInstanceOf[ScalarProductItem]
        val firstScalarProduct = MinimumScalarProductService.scalarProduct(vector1, vector2)
        val result = ScalarProductResult(vector1, vector2, firstScalarProduct, futureResult);
    	println("CONTROLLER: Receiving minimum = " + futureResult.scalarProduct)
    	renderResult(result)
    } catch {
      case t: TimeoutException => Action { implicit request => InternalServerError(t.getMessage())}
    }
  }

  // PRIVATE METHODS
  

  private def renderResult(result: ScalarProductResult) = Action {
    implicit request =>
      render {
        case Accepts.Html() => Ok(views.html.minimumScalarProduct(result))
        case Accepts.Json() => Ok(Json.toJson(result))
      }
  }

  private def string2array(input: String) = input.split(",").map(_.toInt)
}