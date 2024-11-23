# Test Cases

```txt
fun	sqr	x =	( x + 1 ) *	( x - 1 ) \n sqr 4
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
fun fourtimes x = var y = x + x in y + y fourtimes ( fourtimes 16 )
```
