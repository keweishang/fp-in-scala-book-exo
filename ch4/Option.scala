sealed trait Option[+A] {
	def map[B](f: A => B): Option[B] = this match {
		case None => None
		case Some(a) => Some(f(a))
	}

	def flatMap[B](f: A => Option[B]): Option[B] = 
		map(f) getOrElse None

	def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(a) => a
	}

	def orElse[B >: A](ob: => Option[B]): Option[B] = 
		map(Some(_)) getOrElse ob

	def filter(f: A => Boolean): Option[A] = 
		flatMap (a => if (f(a)) Some(a) else None)
}

object Option {
	def mean(xs: Seq[Double]): Option[Double] = 
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)

	def variance(xs: Seq[Double]): Option[Double] = {
		mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
	}
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]