package models

import play.api.libs.json.Json
import play.api.libs.json.JsNumber
import play.api.libs.json.JsObject
import play.api.libs.json.JsString

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