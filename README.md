I'll update the exercises I've finished in the book here.

# Questions

1. Whether or not a function is evaluated depends on where it is called?
- If a function A is passed as the input parameter of another function B, then function A is not evaluated is the input parameter is defined as "lazy (non-strict)" like `a: => A` in the following:
```
def Try[A](a: => A): Option[A] = // a is not evaluated until being referenced
    try Some(a)
    catch { case e: Exception => None }

val age: String = "six"
val optAge: Option[Int] = Try { ago.toInt }
```