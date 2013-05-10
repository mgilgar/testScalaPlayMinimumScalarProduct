package actors

import akka.actor._
import controllers._

class ScalarProductCalculatorWorker extends Actor {
	
	//println("WORKER: Creating worker " + self.path)
  
	def receive = {
	  case Calculate(item, masterActor) =>
	    val result = scalarProduct(item.vector1, item.vector2)
	    //println("WORKER result " + self.path + " = " + result)
	    sender ! new ResultMessage(new ScalarProductItem(item.vector1, item.vector2, result))

	    
	    // I can also use sender
	    //val result = scalarProduct(item.vector1, item.vector2)
	    //self.stop()
	    //self. reply (ResultMessage(ScalarProductItem(item.vector1, item.vector2, result)))
	    //self.stop()
	    context.stop(self)
	} 
	
	private def scalarProduct(vector1: Array[Int], vector2: Array[Int]): Int = {
		vector1
			.zip(vector2)
			.foldLeft(0) { (total, n) => total + n._1 * n._2 }
  }
}