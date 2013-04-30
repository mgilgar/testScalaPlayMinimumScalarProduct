package controllers

import play.api._
import play.api.mvc._
import scala.collection.mutable.ArrayBuffer
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.Json
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.JsNumber
import play.api.libs.json.JsArray

object MinimumScalarProduct extends Controller {
	

	def minimumScalarProduct(input1: String, input2: String) = Action { implicit request =>
		val result = minimumScalarProductResult(input1, input2)
		
   	    render {
		    case Accepts.Html() => Ok(views.html.minimumScalarProduct(result._1, result._2, result._3, result._4))
		    case Accepts.Json() => Ok(Json.obj( "vector1" -> result._1,
		    									"vector2" -> result._2,
		    									"firstScalarProduct" -> result._3,
		    									"minimumScalarProduct" -> Json.obj(
		    									    "vector1" -> result._4._1.mkString(","),
		    									    "vector2" -> result._4._2.mkString(","),
		    									    "result" -> result._4._3)))
		}
	}
	
	private def minimumScalarProductResult(input1: String, input2: String) = {
		val vector1 = input1.split(",").map(_.toInt)
		val vector2 = input2.split(",").map(_.toInt)  
		
		val result = input1.split(",").map(_.toInt)
	    	.zip(input2.split(",").map(_.toInt))
	    	.foldLeft(0) { (total, n) => total + n._1*n._2}
	
		val minScalarProduct = vector1.permutations
				.map({ (p) => (p, vector2, scalarProduct(p, vector2))})
				.foldLeft(new ArrayBuffer[(Array[Int], Array[Int], Int)]) 
				{ (permutationAndScalarProduct2, p) => permutationAndScalarProduct2 += p  }.minBy(_._3)
		(input1, input2, result, minScalarProduct)
	}
	
	private def scalarProduct(vector1 : Array[Int], vector2: Array[Int] ) : Int = {
	  vector1
	    .zip(vector2)
	    .foldLeft(0) { (total, n) => total + n._1*n._2}
	}

}