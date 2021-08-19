import scala.annotation.tailrec
//1. Factorial

def factorial(num: Int): Int = {
  @tailrec
  def helper(n: Int, acc: Int): Int = {
    if( n <= 0 ) acc
    else helper(n-1, acc * n)
  }
  helper(num, 1)
}

factorial(5)


//2. Fibonacci

def fib(num: Int): Int = {
  @tailrec
  def helper(n: Int, m: Int, counter: Int): Int = {
    if(counter == num) n
    else helper(m, n+m, counter + 1)
  }
  helper(0, 1,1)
}

def fibRec(num: Int): Int = {
  if(num <= 1) {
    if(num == 1) 1 else 0
  } else {
    fibRec(num-2) + fibRec(num-1)
  }
}

fib(1)
fib(2)
fib(3)
fib(4)
fib(5)


fibRec(1)
fibRec(2)
fibRec(3)
fibRec(4)
fibRec(5)