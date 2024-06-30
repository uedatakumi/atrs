# atrs

Applicative Term Rewrite System (Naive Implementation)

```
cd src
make
./Main <filename>
```

## Syntax

Variables and Constants

- Variables: x, y, z, ...
- Constants: S, K, I, ...

## Examples

Combinatory Logic (Schönfinkel 1924, Curry 1927)

```
I x = x .
K x y = x .
S x y z = x z (y z) .

MAIN = S K K I .
```

## References

- Baader and Nipkow, Term Rewriting and All That, Cambridge University Press, 1998.

## License

This project is licensed under the MIT License.
