package controllers

import actors._
import akka._
import akka.actor._
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.Json
import play.api.libs.json.JsNumber
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.mvc._
import scala.concurrent.Await
import scala.collection.mutable.ArrayBuffer

case class ScalarProductItem(vector1: Array[Int], vector2: Array[Int], scalarProduct: Int) {
  def toJson: JsObject = {
    Json.obj(
      "vector1" -> vector1.mkString(","),
      "vector2" -> vector2.mkString(","),
      "result" -> scalarProduct)
  }
}

case class ScalarProductResult(input1: String, input2: String, firstScalarProduct: Int, minimumScalarProduct: ScalarProductItem) {
  def toJson: JsObject = {
    Json.obj("vector1" -> input1,
      "vector2" -> input2,
      "firstScalarProduct" -> firstScalarProduct,
      "minimumScalarProduct" -> minimumScalarProduct.toJson)
  }
}

object MinimumScalarProduct extends Controller {

   private val system = ActorSystem("ActorSystem")
  
   def minimumScalarProductUsingOneActor(input1: String, input2: String) = Action { implicit request =>
   	implicit val timeout = Timeout(5000)
   	
    
    val master = system.actorOf(Props(new Actor() {
    	def receive = {
    	  case GetResult =>
    	  	sender ! minimumScalarProductResult(input1, input2)
    	}
      
    }), name = "master")
    
    val future = master ? GetResult
    val result = Await.result(future, timeout.duration ).asInstanceOf[ScalarProductResult]
       render {
      case Accepts.Html() => Ok(views.html.minimumScalarProduct(result))
      case Accepts.Json() => Ok(result.toJson)
    }
   }
  
  
  def minimumScalarProductUsingActors(input1: String, input2: String) = Action { implicit request =>
    implicit val timeout = Timeout(5000)
    
    println
    println
    println
    println
    println
    println
    println
    println
    println
    println
    
    val vector1 = input1.split(",").map(_.toInt)
    val vector2 = input2.split(",").map(_.toInt)
    val master = system.actorOf(Props(new ScalarProductCalculatorMaster(vector1, vector2)))
    val future = master ? GetResult
    val result = Await.result(future, timeout.duration ).asInstanceOf[ScalarProductItem]
    val firstScalarProduct = input1.split(",").map(_.toInt)
      .zip(input2.split(",").map(_.toInt))
      .foldLeft(0) { (total, n) => total + n._1 * n._2 }
    println("CONTROLLER: Receiving minimum = " + result.scalarProduct)
    Ok(views.html.minimumScalarProduct(ScalarProductResult(input1, input2, firstScalarProduct, result)))

    /*
    
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
    }*/
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

  def minimumScalarProductSync(input1: String, input2: String) = Action { implicit request =>
    val r = minimumScalarProductResult(input1, input2)
    render {
      case Accepts.Html() => Ok(views.html.minimumScalarProduct(r))
      case Accepts.Json() => Ok(r.toJson)
    }
  }

  private def minimumScalarProductResult(input1: String, input2: String): ScalarProductResult = {
    val vector1 = input1.split(",").map(_.toInt)
    val vector2 = input2.split(",").map(_.toInt)

    val result = input1.split(",").map(_.toInt)
      .zip(input2.split(",").map(_.toInt))
      .foldLeft(0) { (total, n) => total + n._1 * n._2 }

    val minScalarProduct = vector1.permutations
      .map({ (p) => ScalarProductItem(p, vector2, scalarProduct(p, vector2)) })
      .foldLeft(new ArrayBuffer[ScalarProductItem]) { (permutationAndScalarProduct2, p) => permutationAndScalarProduct2 += p }.minBy(_.scalarProduct)
    ScalarProductResult(input1, input2, result, minScalarProduct)
  }

  private def scalarProduct(vector1: Array[Int], vector2: Array[Int]): Int = {
    vector1
      .zip(vector2)
      .foldLeft(0) { (total, n) => total + n._1 * n._2 }
  }
}