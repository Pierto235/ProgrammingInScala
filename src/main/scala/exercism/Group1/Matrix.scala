package exercism.Group1

case class Matrix(mx: String) {

  private val matrix: Array[Array[Int]] =
    mx.split("\n").map(_.split(" ").map(_.toInt))

  def row(r: Int): Vector[Int] = matrix(r).toVector

  def column(r: Int): Vector[Int] = matrix.transpose.apply(r).toVector

}
