Here is a sample of code that can be run in this interpetor
````
-> def pi = 3.14
ok
-> def square = lambda (x) x * x
ok
-> 2 * square(pi) 
19.7192
-> def bar = {def x = 1; def foo = lambda (z) z + x; foo}
ok
-> bar(3)
4.0
-> x
Undefined identifier: x
-> def a1 = 2 + 3 * 5
ok
-> a1
17.0
-> def a2 = (2 + 3) * 5
ok
-> a2
25.0
-> a1 == a2
false
-> a1 < a2
true
-> a1 < a2 && true && a1 == a2 && a1 == a3
false
-> a1 == a2 || false || a1 < a2 || a1 == a3
true
-> a1 == a3
Undefined identifier: a3
-> def a3 = if (a1 == a2) a4 else 0
ok
-> a3
0.0
-> a4
Undefined identifier: a4
-> def square = lambda (x) x * x
ok
-> square(3)
9.0
-> def f2c = lambda (ft) {def c = 5 / 9; c * (ft - 32)}
ok
-> f2c(212)
100.0
-> c
Undefined identifier: c
-> def addN = lambda (n) lambda (x) x + n
ok
-> def add6 = addN(6)
ok
-> add6(3)
9.0
-> (lambda (z) 2 * z) (3)
6.0
-> def fact = lambda (n) if (n == 0) 1 else n * fact(n – 1)
ok
-> fact(5)
120.0
-> def abs = lambda(x) if (x < 0) -1 * x else x
ok
-> abs(-9)
9.0
-> def delta = 100
ok
-> def small = {def delta = 0.00001; lambda(x) abs(x) < delta}
ok
-> small(-0.00000001)
true
-> small(0.001)
false
````
