package controllers

import play.api.mvc.WebSocket
import play.api.libs.iteratee.Iteratee
import play.api.libs.iteratee.Enumerator
import play.api._
import play.api.mvc._

object Echo extends Controller {
  def processEcho = WebSocket.using[String] { request =>

    // Send a single 'Hello!' message
    val out = Enumerator.imperative[String]()
    
    // Log events to the console
    val in = Iteratee.foreach[String]({
    			msg =>
    			  println(msg)
    			  out.push ("Received: " + msg)
    		}
        ).mapDone { _ =>
      println("Disconnected")
    }


    (in, out)
  }  
  
  private def processInput(input: String) = {
    
  }
  
  def viewEchoPage = Action {
    Ok(views.html.echo())
  }
  
}