package controllers

import play.api._
import play.api.mvc._
import scala.collection.mutable.ArrayBuffer

object MinimumScalarProduct extends Controller {
	
	def scalarProduct(vector1 : Array[Int], vector2: Array[Int] ) : Int = {
	  vector1
	    .zip(vector2)
	    .foldLeft(0) { (total, n) => total + n._1*n._2}
	}

	def doCalculate(input1: String, input2: String) = Action {
		val vector1 = input1.split(",").map(_.toInt)
		val vector2 = input2.split(",").map(_.toInt)  
		
		val result = input1.split(",").map(_.toInt)
	    	.zip(input2.split(",").map(_.toInt))
	    	.foldLeft(0) { (total, n) => total + n._1*n._2}
	
		val minScalarProduct = vector1.permutations
				.map({ (p) => (p, vector2, scalarProduct(p, vector2))})
				.foldLeft(new ArrayBuffer[(Array[Int], Array[Int], Int)]) 
				{ (permutationAndScalarProduct2, p) => permutationAndScalarProduct2 += p  }.minBy(_._3)
	
		Ok(views.html.doCalculate(input1, input2, result, minScalarProduct))
  }
}