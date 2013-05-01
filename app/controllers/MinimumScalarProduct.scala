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

case class MinimimScalarProductResult(minScalarProduct: (Array[Int], Array[Int], Int)) 
case class ScalarProductResult(input1: String, input2: String, firstScalarProduct: Int, minimumScalarProduct: MinimimScalarProductResult)


object MinimumScalarProduct extends Controller {

    
  
  def minimumScalarProductSync(input1: String, input2: String) = Action { implicit request =>
    val result = minimumScalarProductResult(input1, input2)

    render {
      case Accepts.Html() => Ok(views.html.minimumScalarProduct(result.input1, result.input2, result.firstScalarProduct, (result.minimumScalarProduct.minScalarProduct._1, result.minimumScalarProduct.minScalarProduct._2, result.minimumScalarProduct.minScalarProduct._3)))
      case Accepts.Json() => Ok(Json.obj("vector1" -> result.input1,
        "vector2" -> result.input2,
        "firstScalarProduct" -> result.firstScalarProduct,
        "minimumScalarProduct" -> Json.obj(
          "vector1" -> result.minimumScalarProduct.minScalarProduct._1.mkString(","),
          "vector2" -> result.minimumScalarProduct.minScalarProduct._2.mkString(","),
          "result" -> result.minimumScalarProduct.minScalarProduct._3)))
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
        //case r: (String, String, Int, (Array[Int], Array[Int], Int)) =>
        case r: ScalarProductResult =>
          render {
            case Accepts.Html() => Ok(views.html.minimumScalarProduct(r.input1, r.input2, r.firstScalarProduct, r.minimumScalarProduct.minScalarProduct))
            case Accepts.Json() => Ok(Json.obj("vector1" -> r.input1,
              "vector2" -> r.input2,
              "firstScalarProduct" -> r.firstScalarProduct, 
              "minimumScalarProduct" -> Json.obj(
                "vector1" -> r.minimumScalarProduct.minScalarProduct._1.mkString(","),
                "vector2" -> r.minimumScalarProduct.minScalarProduct._2.mkString(","),
                "result" -> r.minimumScalarProduct.minScalarProduct._3)))
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
      .map({ (p) => (p, vector2, scalarProduct(p, vector2)) })
      .foldLeft(new ArrayBuffer[(Array[Int], Array[Int], Int)]) { (permutationAndScalarProduct2, p) => permutationAndScalarProduct2 += p }.minBy(_._3)
    ScalarProductResult(input1, input2, result, MinimimScalarProductResult(minScalarProduct))
  }

  private def scalarProduct(vector1: Array[Int], vector2: Array[Int]): Int = {
    vector1
      .zip(vector2)
      .foldLeft(0) { (total, n) => total + n._1 * n._2 }
  }

}