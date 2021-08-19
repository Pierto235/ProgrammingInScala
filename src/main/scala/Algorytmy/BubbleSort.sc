import scala.util.Random

def bubbleSort(arr: Array[Int]): Array[Int] = {

  val l = arr.length
  var k = l
  for(m <- 0 to l-1) {
    for(n <- 0 to k - 2) {
     val temp = arr(n)
     if(temp > arr(n+1)){
       arr(n) = arr(n+1)
       arr(n+1) = temp
     }
    }
    k = k-1
  }
  arr
}

val arr = Array.fill(20)(Random.nextInt(100))
bubbleSort(arr)

def bubbleSort2(arr: Array[Int]): Array[Int] = {
  val l = arr.length
  for(m <- 0 until l-1; n <- 0 until l - 1 - m  ) {
    if(arr(n) > arr(n+1)){
      val temp = arr(n)
      arr(n) = arr(n+1)
      arr(n+1) = temp
    }
  }
  arr
}
bubbleSort2(arr)

def bubbleSort3(arr: Array[Int]): Array[Int] = {
  val l = arr.length
  (0 until l-1).foreach( m => (0 until l - 1 - m ).foreach(n =>
    if(arr(n) > arr(n+1)){
      val temp = arr(n)
      arr(n) = arr(n+1)
      arr(n+1) = temp
    }
  ))
  arr
}
bubbleSort3(arr).mkString(", ")

















