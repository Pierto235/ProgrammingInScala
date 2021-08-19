package Futures

import java.util.concurrent.Executors

object Intro extends App {
  // JVM threads

  // thread is an instance of a class

  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Running in parallel")
  })

  aThread.start() //gives the signal to the JVM to start a JVM thread
  //create a JVM thread => os thread
  aThread.join()  //blocks until aThread finishes running


  val threadHello = new Thread(() => (1 to 5).foreach(_=> println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_=> println("goodbye")))

  threadHello.start()
  threadGoodbye.start()
  //different runs produce different results

  //executors
  val pool = Executors.newFixedThreadPool(10)

  pool.execute(() => println("something in the thread pool"))

  pool.execute(() => {
    Thread.sleep(1000)
    println("done after 1 second")
  })

  pool.execute(() => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("done after 2 seconds")
  })

  //pool.shutdown() //no more action can be submitted, but actions which are running will fefinished
  //pool.shutdownNow() // stops everything
}
