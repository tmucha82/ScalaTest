def general(neutral: Int, g: (Int, Int) => Int)(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) neutral
  else g(f(a), general(neutral, g)(f)(a + 1, b))
}

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  general(0, (x, y) => x + y)(f)(a, b)
}

def product(f: Int => Int)(a: Int, b: Int): Int = {
  general(1, (x, y) => x * y)(f)(a, b)
}

def fact(n: Int): Int = product(x => x)(1, n)

sum(x => x)(1, 4)
product(x => x)(1, 4)