sealed trait List[+A]

// data constructors
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x,xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1
		case Cons(0, _) => 0
		case Cons(x, xs) => x * product(xs)
	}

	def apply[A](as: A*): List[A] =
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

	def tail[A](l: List[A]) : List[A] = l match {
		case Nil => sys.error("tail of empty list")
		case Cons(_, t) => t
	}

	def setHead[A](l: List[A], a: A): List[A] = l match {
		case Nil => sys.error("set head of Nil")
		case Cons(_, t) => Cons(a, t)
	}

	def drop[A](l: List[A], n: Int): List[A] =
		if (n == 0) l;
		else l match {
			case Nil => Nil
			case Cons(_, t) => drop(t, n - 1)
		}

	/* 
	Somewhat overkill, but to illustrate the feature we're using a _pattern guard_,
	to only match a `Cons` whose head satisfies our predicate, `f`. 
	The syntax is to add `if <cond>` after the pattern, before the `=>`, 
	where `<cond>` can use any of the variables introduced by the pattern.
	*/
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
		case Cons(h, t) if (f(h)) =>  dropWhile(t, f)
		case _ => l
	}

	def init[A](l: List[A]): List[A] = l match {
		case Nil => sys.error("no init for Nil")
		case Cons(_, Nil) => Nil
		case Cons(h, t) => Cons(h, init(t))
	}

	/*
	Note that we're copying the entire list up until the last element. Besides being inefficient, the natural recursive solution will use a stack frame for each element of the list, which can lead to stack overflows for large lists (can you see why?). With lists, it's common to use a temporary, mutable buffer internal to the function (with lazy lists or streams, which we discuss in chapter 5, we don't normally do this). So long as the buffer is allocated internal to the function, the mutation is not observable and RT is preserved.

	Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which doesn't require even local mutation. We'll write a reverse function later in this chapter.
	*/
	def init2[A](l: List[A]): List[A] = {
		import collection.mutable.ListBuffer
		val buf = new ListBuffer[A]
		@annotation.tailrec
		def go(cur: List[A]): List[A] = cur match {
			case Nil => sys.error("no init for Nil")
			case Cons(_, Nil) => List(buf.toList: _*)
			case Cons(h, t) => buf += h; go(t)
		}
		go(l)
	}

	// not tail-recursive, a.k.a not stack-safe
	def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs, z)(f))
		}

	def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

	def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

	def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

	@annotation.tailrec
	def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
	}

	def sum3(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

	def product3(xs: List[Double]): Double = foldLeft(xs, 1.0)(_ * _)

	def length2[A](xs: List[A]): Int = foldLeft(xs, 0)((acc, _) => acc + 1)

	def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

	def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
		foldLeft(reverse(l), z)((acc, h) => f(h, acc))

	def append[A](l: List[A], r: List[A]): List[A] =
		foldRight(l, r)(Cons(_, _))

	// Note that we're simply referencing the `append` function, without writing something like `(x,y) => append(x,y)` or `append(_,_)`. In Scala there is a rather arbitrary distinction between functions defined as _methods_, which are introduced with the `def` keyword, and function values, which are the first-class objects we can pass to other functions, put in collections, and so on. This is a case where Scala lets us pretend the distinction doesn't exist. In other cases, you'll be forced to write `append _` (to convert a `def` to a function value) or even `(x: List[A], y: List[A]) => append(x,y)` if the function is polymorphic and the type arguments aren't known. 
	def concat[A](ls: List[List[A]]): List[A] =
		foldRight(ls, Nil: List[A])(append)


}