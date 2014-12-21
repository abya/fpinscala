package fpinscala.gettingstarted

class Tests {
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // my version
  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = {
    def foo(b: B): C = f(a, b)
    foo
  }

  //my version
  def curry1[A, B, C](f: (A, B) => C): A => (B => C) = {
    def foo(a: A) = (b: B) => f(a, b)
    foo
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  //my version
  def uncurry1[A,B,C](f: A => B => C): (A,B) => C = {
    def foo(a: A, b: B) = f(a)(b)
    foo
  }
    
  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a,b) => f(a)(b)
    
    
}