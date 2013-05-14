package services

import models._

object MinimumScalarProductService {
  def scalarProduct(vector1: Array[Int], vector2: Array[Int]): Int = {
    vector1
      .zip(vector2)
      .foldLeft(0) { (total, n) => total + n._1 * n._2 }
  }
  
  def minimumScalarProductResult(vector1: Array[Int], vector2: Array[Int]): ScalarProductResult = {
    minimumScalarProductResult(vector1, vector2, scalarProductOfAllPermutations(vector1, vector2))
  }
  
  def minimumScalarProductResult(vector1: Array[Int], vector2: Array[Int], scalarProductAllPermutations: Iterator[ScalarProductItem]): ScalarProductResult = {
    val result = MinimumScalarProductService.scalarProduct(vector1, vector2) 

    val minScalarProduct = scalarProductAllPermutations
      // This line provides all possible permutations, data that we are not printing but I may in the future
      //.foldLeft(new ArrayBuffer[ScalarProductItem]) { (permutationAndScalarProduct2, p) => permutationAndScalarProduct2 += p }
      .minBy(_.scalarProduct)
    
      // TODO: no much sense in returning vector1 and vector2 as we are passing them as parameters
      ScalarProductResult(vector1, vector2, result, minScalarProduct)
  }
  
  def scalarProductOfAllPermutations(vector1: Array[Int], vector2: Array[Int]) : Iterator[ScalarProductItem] = {
    vector1.permutations
      .map({ (p) => ScalarProductItem(p, vector2, MinimumScalarProductService.scalarProduct(p, vector2)) })
  }

}