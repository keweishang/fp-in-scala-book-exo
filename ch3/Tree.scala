sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
	def size(t: Tree[A]): Int = t match {
		case Leaf(_) => 1
		case Branch(l, r) => 1 + size(l) + size(r)
	}

	/*
	We're using the method `max` that exists on all `Int` values rather than an explicit `if` expression.

	Note how similar the implementation is to `size`. We'll abstract out the common pattern in a later exercise. 
	*/
	def maximum(t: Tree[Int]): Int = t match {
		case Leaf(v) => v
		case Branch(l, r) => maximum(l) max maximum(r)
	}

	def depth(t: Tree[A]): Int = t match {
		case Leaf(_) => 0
		case Branch(l, r) => (depth(l) max depth(r)) + 1
	}

	def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
		case Leaf(v) => Leaf(f(v))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}

	def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
		case Leaf(v) => f(v)
		case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
	}

	def sizeViaFold(t: Tree[A]): Int =
		fold(t)(a => 1)(1 + _ + _)

	def maximumViaFold(t: Tree[Int]): Int =
		fold(t)(a => a)(_ max _)

	def depthViaFold(t: Tree[A]): Int =
		fold(t)(a => 0)((d1, d2) => (d1 max d2) + 1)

	/*
	Note the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error like this: 

	type mismatch;
	  found   : fpinscala.datastructures.Branch[B]
	  required: fpinscala.datastructures.Leaf[B]
	     fold(t)(a => Leaf(f(a)))(Branch(_,_))
	                                    ^  

	This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to infer `Tree[B]` as the result type in both cases. When working with algebraic data types in Scala, it's somewhat common to define helper functions that simply call the corresponding data constructors but give the less specific result type:  
	  
	  def leaf[A](a: A): Tree[A] = Leaf(a)
	  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
	*/
	def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = 
	  fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}