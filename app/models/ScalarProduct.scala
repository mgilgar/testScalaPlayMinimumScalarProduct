package models

import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Writes

case class ScalarProductItem(vector1: Array[Int], vector2: Array[Int], scalarProduct: Int)

object ScalarProductItem {
	implicit val tournamentWrites = new Writes[ScalarProductItem] {
	    def writes(scalarProductItem: ScalarProductItem): JsValue = {
	      Json.obj(
	      "vector1" -> scalarProductItem.vector1.mkString(","),
	      "vector2" -> scalarProductItem.vector2.mkString(","),
	      "result" -> scalarProductItem.scalarProduct)
	    }
	}
}

case class ScalarProductResult(vector1: Array[Int], vector2: Array[Int], firstScalarProduct: Int, minimumScalarProduct: ScalarProductItem)

object ScalarProductResult {
	implicit val tournamentWrites = new Writes[ScalarProductResult] {
	    def writes(scalarProductResult: ScalarProductResult): JsValue = {
	      Json.obj("vector1" -> scalarProductResult.vector1.mkString(","),
      "vector2" -> scalarProductResult.vector2.mkString(","),
      "firstScalarProduct" -> scalarProductResult.firstScalarProduct,
      "minimumScalarProduct" -> Json.toJson(scalarProductResult.minimumScalarProduct))
	    }
	}
}