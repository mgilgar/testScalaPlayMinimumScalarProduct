package models

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.immutable.Vector

import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

import anorm.Pk
import play.api.libs.json._

class ScalarProductSpec extends Specification with Mockito {

  val scalarProductItem = ScalarProductItem(Array(1, 2), Array(3, 4), 0)
  val scalarProductResult = ScalarProductResult(Array(4, 5), Array(3, 6), 0, scalarProductItem)

  val jsonScalarProductItem = Json.toJson(scalarProductItem)

  "Calling Json.toJson(scalarProductItem)" should {
    "return a JsValue object" in {
      jsonScalarProductItem must beAnInstanceOf[JsValue]
    }
    "vector1 should be String '1,2'" in {
      (jsonScalarProductItem \ "vector1") must beAnInstanceOf[JsString]
      (jsonScalarProductItem \ "vector1").as[JsString] must be equalTo (JsString("1,2"))
    }
    "vector2 should be a String '3,4'" in {
      (jsonScalarProductItem \ "vector2") must beAnInstanceOf[JsString]
      (jsonScalarProductItem \ "vector2").as[JsString] must be equalTo (JsString("3,4"))
    }
    "result should be 0" in {
      (jsonScalarProductItem \ "result" must beAnInstanceOf[JsNumber])
      (jsonScalarProductItem \ "result").as[JsNumber] must be equalTo (JsNumber(0))
    }
  }
  val jsonScalarProductResult = Json.toJson(scalarProductResult)
  "Calling Json.toJson(scalarProductResult)" should {
    "return a JsValue object" in {
      jsonScalarProductResult must beAnInstanceOf[JsValue]
    }
    "vector1 should be String '4,5'" in {
      (jsonScalarProductResult \ "vector1") must beAnInstanceOf[JsString]
      (jsonScalarProductResult \ "vector1").as[JsString] must be equalTo (JsString("4,5"))
    }
    "vector2 should be a String '3,6'" in {
      (jsonScalarProductResult \ "vector2") must beAnInstanceOf[JsString]
      (jsonScalarProductResult \ "vector2").as[JsString] must be equalTo (JsString("3,6"))
    }
    "firstScalarProduct should be 0" in {
      (jsonScalarProductResult \ "firstScalarProduct" must beAnInstanceOf[JsNumber])
      (jsonScalarProductResult \ "firstScalarProduct").as[JsNumber] must be equalTo (JsNumber(0))
    }
  }
}