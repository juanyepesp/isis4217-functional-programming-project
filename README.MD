# Functional Programming Project

- Maria Castro Iregui - 202020850 
- Esteban Gonzalez Ruales - 202021225
- Juan Diego Yepes - 202022391

Our project parses and executes the functional programming language proposed as a project. Everything was developed in the `main.oz` file, which executes the test cases proposed in class. 

As stated in the project clarification, we decided to use parentheses, which have to be used like this, **with spaces in between each character**:

- For arithmethic clarification: 
    - `( x - 1 ) * ( x + 1 )`
- For calling a function within a function (not in function definition, only in function calling): 
    - `sqr ( sqr ( sqr 3 ) )`
    - `sum_n 1 2 ( sum_n 1 2 3 ) 3`

You have to write a newline `\n` character, with single spaces, to separate the function definition and function call.

# Test Cases

```txt
fun sqr x = ( x + 1 ) * ( x - 1 ) \n sqr 4
```

```txt
fun	sqr	x =	( x + 1 ) *	( x - 1 ) \n sqr 4
```

```txt
fun	twice x	= x	+ x \n twice 5
```

```txt
fun square x = x * x \n square 3
```

```txt
fun square x = x * x \n square ( square 3 )
```

```txt
fun sum_n x y z n = ( x + y + z ) * n \n sum_n 1 ( sum_n 1 1 1 2 ) 3 2
```

```txt
fun arithmetic x y = ( ( x + y ) / ( x - y ) ) * 2 \n arithmetic ( arithmetic 5 6 ) ( arithmetic 2 11 )
```

```txt
fun fourtimes x = var y = x + x in y + y \n fourtimes ( fourtimes 16 )
```
