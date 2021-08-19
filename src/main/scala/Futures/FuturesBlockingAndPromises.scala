package Futures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, Promise}
import scala.util.Success

object FuturesBlockingAndPromises extends App {


  //online banking app
  case class User(name:String)
  case class Transaction(sender: String, receiver: String, amount: Double, status:String)

  object BankingApp {
    val name = "Rock the JVM banking"
    def fetchUser(name: String): Future[User] = Future {
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount:Double): Future[Transaction] = Future {
      Thread.sleep(1000)
      Transaction(user.name,merchantName, amount, "SUCCESS")

    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      // fetch the user from the DB
      // create a transaction
      // WAIT for the transaction to finish

      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield { transaction.status}

      Await.result(transactionStatusFuture, 2.seconds)    //implicit conversions
    }
  }

  println(BankingApp.purchase("Daniel", "iphone12", "rock the JVM store", 3000))


  //promises

  val promise = Promise[Int]() // "controlling" over future
  val future = promise.future

  // thread 1 - "consumer"
  future.onComplete {
    case Success(value) => println("[consumer] I ve received " + value)
  }

  // thread 2 - "producer"
  val producer = new Thread(() => {
    println("[producer] crushing numbers ...")
    Thread.sleep(500)
    //fulfiling th promise
    promise.success(42)
    println(("[producer] done"))
    })

  producer.start()
  Thread.sleep(1000)

}
