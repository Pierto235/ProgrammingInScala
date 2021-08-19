package WzorceProjektowe

import scala.collection.mutable.ListBuffer

object TheCommandDesignPattern extends App {


  //Receiver
  case class Robot() {
    def cleanUp(): Unit = println("Cleaning up.")
    def pourJuice(): Unit = println("Pouring juice.")
    def makeSandwich(): Unit = println("Making a sandwich.")
  }


  //Command
  trait RobotCommand {
    def execute(): Unit
  }

  case class MakeSandwichCommand(robot: Robot) extends RobotCommand {
    override def execute(): Unit = robot.makeSandwich()
  }

  case class PourJuiceCommand(robot: Robot) extends RobotCommand {
    override def execute(): Unit = robot.pourJuice()
  }

  case class CleanUpCommand(robot: Robot) extends RobotCommand {
    override def execute(): Unit = robot.cleanUp()
  }


  //Robot Controller
  class RobotController {
    val history = ListBuffer[RobotCommand]()

    def issueCommand(command: RobotCommand): Unit = {
      command +=: history
      command.execute()
    }

    def showHistory(): Unit = {
      history.foreach(println)
    }
  }


  //Main
  val robot = Robot()
  val robotController = new RobotController
  robotController.issueCommand(MakeSandwichCommand(robot))
  robotController.issueCommand(PourJuiceCommand(robot))
  System.out.println("I'm eating and having some juice.")
  robotController.issueCommand(CleanUpCommand(robot))
  System.out.println("Here is what I asked my robot to do:")
  robotController.showHistory()



}
