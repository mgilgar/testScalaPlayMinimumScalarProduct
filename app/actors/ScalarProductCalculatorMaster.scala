package actors

import actors._
import akka._
import akka.actor._
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import controllers._
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.CountDownLatch
import scala.collection.mutable.ArrayBuffer
import scala.concurrent._
import ExecutionContext.Implicits.global

class ScalarProductCalculatorMaster(vector1: Array[Int], vector2: Array[Int]) extends Actor {
	
  implicit val timeout = Timeout(5000)
  //implicit val ec = ExecutionContext.fromExecutorService(yourExecutorServiceGoesHere)
  //implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(50))
  
  private implicit val system = ActorSystem("ActorSystem")
	private var permutations = vector2.permutations.toList
	private var results = new Array[ScalarProductItem](permutations.length)
	private var counter: AtomicInteger = new AtomicInteger(permutations.length)
	//private var latch = new CountDownLatch(permutations.length)
    
	//val minScalarProduct = permutations
	//val minScalarProduct = vector2.permutations
    
    /*(List.range(0, permutations.length), permutations).zipped.foreach(
        { (i, p) => 
        	println("MASTER: creating worker for permutation i = " + i)
        	val actor = system.actorOf(Props(new ScalarProductCalculatorWorker()), name = "worker" + i)
    		println("MASTER: Sending worker i = " + i + " Calculate message")
    		actor ! Calculate(ScalarProductItem(vector1, p, 0), self)
        })*/
    
   def receive = {
   		case GetResult => 
   		
   		  println("MASTER: asked for the result")
   		  val listOfFutures : List[scala.concurrent.Future[ResultMessage]] = 
   		  permutations.map(
   		{ (p) => 
        	//println("MASTER: creating worker for permutation" )
        	val actor = system.actorOf(Props(new ScalarProductCalculatorWorker()))
        	akka.pattern.ask(actor, Calculate(ScalarProductItem(vector1, p, 0), self)).mapTo[ResultMessage]
        })
        
        val futureList : Future[List[ResultMessage]] = Future.sequence(listOfFutures) 
   		  
        val results = Await.result(futureList, timeout.duration).asInstanceOf[List[ResultMessage]]
   		val min = results.minBy(_.item.scalarProduct)
   		
   		println("MASTER: min = " + min.item.scalarProduct)
   		sender ! min.item
   		context.stop(self)
   }
/*        
   def receive2 = {
	  //case Start =>     
	    //val minScalarProduct = permutations
    	//.map( { (p) =>
    	  //val actor = system.actorOf(Props(new ScalarProductCalculatorWorker()), name = "worker" + p.toString())
    	  //actor ! Calculate(ScalarProductItem(vector1, p, 0), self)})
	  //case Result(item, workerActor) =>
      case ResultMessage(r) =>
	    println("MASTER: I am receiving a ResultMEssage")
        val index = counter.decrementAndGet()
        results(index) = r
	    println("MASTER: I received: " + r.scalarProduct + " from " + sender.path + " result number = " + index)

	    //latch.countDown();
	    
	    if (counter == permutations.length) {
	      println("MASTER: parando")
	      context.stop(self)
	     // results.foldLeft(new ArrayBuffer[ScalarProductItem]) { (permutationAndScalarProduct2, p) => permutationAndScalarProduct2 += p }.minBy(_.scalarProduct)
	    }
	  case GetResult =>
	    println("MASTER: Result asked")
	    //latch.await();
	    val toReturn = results
	    	.foldLeft(new ArrayBuffer[ScalarProductItem]) { (permutationAndScalarProduct2, p) => permutationAndScalarProduct2 += p }.minBy(_.scalarProduct)
	    
	    println("MASTER: result: " + toReturn.scalarProduct)
	    sender ! toReturn
	  case _ =>
	    println("MASTER: message received but is unknown")
	}*/
}