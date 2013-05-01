package controllers

import play.api._
import play.api.mvc._
import scala.collection.mutable.ArrayBuffer
import play.api.libs.concurrent.Execution.Implicits._
import play.api._
import play.api.mvc._
import scala.collection.mutable.ArrayBuffer
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.Json
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.JsNumber
import play.api.libs.json.JsArray

case class ScalarProductItem(vector1: Array[Int], vector2: Array[Int], scalarProduct: Int) 
case class ScalarProductResult(input1: String, input2: String, firstScalarProduct: Int, minimumScalarProduct: ScalarProductItem)


object MinimumScalarProduct extends Controller {

  def minimumScalarProductSync(input1: String, input2: String) = Action { implicit request =>
    val result = minimumScalarProductResult(input1, input2)

    render {
      case Accepts.Html() => Ok(views.html.minimumScalarProduct(result.input1, result.input2, result.firstScalarProduct, result.minimumScalarProduct))
      case Accepts.Json() => Ok(toJson(result))
    }
  }

  def minimumScalarProductAsync(input1: String, input2: String) = Action { implicit request =>

    val futureResult: scala.concurrent.Future[ScalarProductResult] =
      scala.concurrent.Future[ScalarProductResult] {
        minimumScalarProductResult(input1, input2)
      }
    val timeoutFuture = play.api.libs.concurrent.Promise.timeout("Oops", 5000)

    Async {
      // We are calling a method in the companion object of Future trait.
      scala.concurrent.Future.firstCompletedOf(Seq(futureResult, timeoutFuture)).map {
        case r: ScalarProductResult =>
          render {
            case Accepts.Html() => Ok(views.html.minimumScalarProduct(r.input1, r.input2, r.firstScalarProduct, r.minimumScalarProduct))
            case Accepts.Json() => Ok(toJson(r))
          }
        case t: String => InternalServerError(t)
      }
    }
  }
  
  private def minimumScalarProductResult(input1: String, input2: String) : ScalarProductResult = {
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
  
  private def toJson(scalarProductResult : ScalarProductResult) : JsObject = {
	  Json.obj("vector1" -> scalarProductResult.input1,
        "vector2" -> scalarProductResult.input2,
        "firstScalarProduct" -> scalarProductResult.firstScalarProduct,
        "minimumScalarProduct" -> toJson(scalarProductResult.minimumScalarProduct))   
  }
  
  private def toJson(scalarProductItem: ScalarProductItem) : JsObject = {
    Json.obj(
                "vector1" -> scalarProductItem.vector1.mkString(","),
                "vector2" -> scalarProductItem.vector2.mkString(","),
                "result" -> scalarProductItem.scalarProduct)
  }

}