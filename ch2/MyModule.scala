/** A documentation comment */
object MyModule {
	def abs(n: Int): Int =
		if (n < 0) -n
		else n

	def factorial(n: Int): Int = {
		@annotation.tailrec
		def go(n: Int, acc: Int): Int = 
			if (n <= 0) acc
			else go(n - 1, n * acc);
		go(n, 1)
	}

	private def formatAbs(x: Int) = {
		val msg = "The absolute value of %d is %d."
		msg.format(x, abs(x))
	}

	private def formatFactorial(n: Int) = {
		val msg = "The factorial of %d is %d."
		msg.format(n, factorial(n))
	}

	def formatResult(name: String, n: Int, f: Int => Int) = {
		val msg = "The %s of %d is %d."
		msg.format(name, n, f(n))
	}

	def main(args: Array[String]): Unit = {
		println(formatAbs(-42))
		println(formatFactorial(7))
	}

	def fib(n: Int): Int = {
		@annotation.tailrec
		def go(n: Int, prev: Int, cur: Int): Int = {
			if (n == 0) prev
			else go(n-1, cur, cur + prev)
		}
		go(n, 0, 1)
	}

	def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
		@annotation.tailrec
		def loop(n: Int): Boolean = {
			if (n == as.length - 1) return true
			else if (gt(as(n), as(n+1))) false
			else loop(n + 1)
		}

		loop(0)
	}

	def curry[A,B,C](f: (A, B) => C): A => (B => C) =
		a => b => f(a,b)


	def uncurry[A,B,C](f: A => B => C) : (A, B) => C =
		(a, b) => f(a)(b)


	def compose[A,B,C](f: B => C, g: A => B): A => C =
		a => f(g(a))
}