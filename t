I := \x.x
Zero := \f.\x.x
One := \f.\x.(f) x
Two := \f.\x.(f)(f)x
Const := \x.\y.x

Main := ((Const) y) y
