P := \m.\n.\f.\x.((m f) ((n f) x))
Z := \f.\x.x
O := \f.\x.(f x)
Norm := \n.((n s) z)

TRUE := \x.\y.x
FALSE := \x.\y.y
IF := \p.\a.\b.((p a) b)

PAIR := \x.\y.\f.((f x) y)
FIRST := \p.(p TRUE)
SECOND := \p.(p FALSE)
NIL := \x.TRUE
NULL := \p.(p \x.\y.FALSE)
Y := \g.(\x.(g (x x)) \x.(g (x x)))

Ls := ((PAIR O) ((PAIR O) ((PAIR O) NIL)))
SumImpl := \n.\a.(((IF (NULL a)) Z) ((P (FIRST a)) (n (SECOND a))))
Sum := (Y SumImpl)

Main := (Norm (Sum Ls))
