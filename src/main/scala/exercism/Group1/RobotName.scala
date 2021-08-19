package exercism.Group1

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random


object Robot {
  val letters = ('A' to 'Z').toList
  val numbers = (0 to 9).toList

  val codeGenTuple = (Random.shuffle(letters),
    Random.shuffle(letters),
    Random.shuffle(numbers),
    Random.shuffle(numbers),
    Random.shuffle(numbers))

  val codes = for {
    l1 <- codeGenTuple._1
    l2 <- codeGenTuple._2
    n1 <- codeGenTuple._3
    n2 <- codeGenTuple._4
    n3 <- codeGenTuple._5
  } yield (l1.toString + l2.toString + n1 + n2 + n3)

  var unusedCodes = codes

  def getCode(): String = {
    val code = unusedCodes.head.mkString
    unusedCodes = unusedCodes.tail
    code
  }

  def addRemoved(name: String): Unit = {
    unusedCodes = unusedCodes ::: List(name)
  }

}

class Robot {

  var name = Robot.getCode()

  def reset(): Unit = {
    Robot.addRemoved(name)
    name = Robot.getCode()
  }

}

////////////////////////////////////////////////////////////////////////////////////////////////
// Next version

object Robot1 {
  val alreadySet = mutable.HashSet.empty[String]
  def nameExist(name: String): Boolean = {
    if(!alreadySet.contains(name)) {
      alreadySet += name
      false
    } else true
  }
}


class Robot1 {

  val letters = 'A' to 'Z'

  def generateLetters: String = (1 to 2).foldLeft("")((a,_) => a + letters(Random.nextInt(letters.length)))
  def generateNumbers: String = (1 to 3).foldLeft("")((a, _) => a + Random.nextInt(9))

  def generateName(): String = generateLetters + generateNumbers

  var name: String = {
    @tailrec
    def helper(name: String): String = {
      Robot1.nameExist(name) match {
        case true => helper(generateName())
        case false => name
      }
    }
    helper(generateName())

  }

  def reset(): Unit = name = generateName

}

