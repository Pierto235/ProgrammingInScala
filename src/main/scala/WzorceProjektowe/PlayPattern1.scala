package WzorceProjektowe

object PlayPattern1 extends App {

  class ModelOneController extends BaseController {
    val modelForm: String = "Forma"
    override val editionForm: String = "Edycja"

    val modelParams = new ModelParams( "modelOne", "descriptionOne", "additionOne", "saveTitleOne")

  }

  class ModelTwoController extends BaseController {
    val modelForm: String = "FormaTwo"
    override val editionForm: String = "EdycjaTwo"

    val modelParams = new ModelParams( "modelTwo", "descriptionTwo", "additionTwo", "saveTitleTwo")

  }


  abstract class BaseController {

    case class ModelParams(
                            name: String,
                            description: String,
                            additionTitle: String,
                            saveTitle: String
                          )

    val modelParams: ModelParams

    val modelForm: String
    val editionForm: String

    def add(): String = {
      s"${modelParams.additionTitle} dodaje model ${modelParams.name} z opisem ${modelParams.description} "
    }

    def save(id: Int): Unit = println(s"${modelParams.saveTitle} zapisuje do bazy model ${modelParams.name}")

  }


  val a = new ModelOneController
  println(a.add())
  a.save(1)

  val b = new ModelTwoController
  println(b.add())
  b.save(1)

}
