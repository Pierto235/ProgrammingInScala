package Futures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}

object FutureAndPromises extends App {

  def calculateTheMiningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture:Future[Int] = Future {        //calling apply for the futures
    calculateTheMiningOfLife                //calculates the mining of life on another thread
  } //(ec:global)

  println("waiting on the future")
  aFuture.onComplete( t => t match {
    case Success(value) => println(s"the mining of life is $value")
    case Failure(exception) => println(s"I have failed with $exception")
  })  // we dont know which thread will execute this function
  Thread.sleep(3000)




  // mini social network

  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile) =
      println(s"${this.name} poking ${anotherProfile.name}")
  }

  object SocialNetwork {
    //database
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy"
    )
    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )

    //API

    val random = new Random()
    def fetchProfile(id: String): Future[Profile] = Future {
      Thread.sleep(random.nextInt(3000))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(4000))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }


  //client:mark to poke bill

  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")

//  mark.onComplete{
//    case Success(markProfile) => {
//      val bill = SocialNetwork.fetchBestFriend(markProfile)
//      bill.onComplete{
//        case Success(billProfile) => markProfile.poke(billProfile)
//        case Failure(e) => e.printStackTrace()
//      }
//    }
//    case Failure(e) => e.printStackTrace()
//  }

 // Thread.sleep(1000)


  // Functional composition of futures
  //map, flatMap , filter

  val nameOnTheWall = mark.map(profile => profile.name)
  val marksBestFriend = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val zucksBestFriendRestricted = marksBestFriend.filter(profile => profile.name.startsWith("z"))

  // for - comprehensions

  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(mark)
  } yield {mark.poke(bill)}

  Thread.sleep(1000)

  //fallbacks

  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("fb.id.0-dummy", "Forever alone")
  }

  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val fallbackResult = SocialNetwork.fetchProfile("unknown id").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))

}
