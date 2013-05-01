package controllers

import play.api.mvc.WebSocket
import play.api.libs.iteratee.Iteratee
import play.api.libs.iteratee.Enumerator
import play.api._
import play.api.mvc._

object Echo extends Controller {
  def processEcho = WebSocket.using[String] { request =>

    // Log events to the console
    val in = Iteratee.foreach[String](println).mapDone { _ =>
      println("Disconnected")
    }

    // Send a single 'Hello!' message
    val out = Enumerator("Hola Websockets!")

    (in, out)
  }  
  
  def viewEchoPage = Action {
    Ok(views.html.echo())
  }
  
}