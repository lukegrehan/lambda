P := \m.\n.\f.\x.((m f) ((n f) x))
Z := \f.\x.x
O := \f.\x.(f x)
T := ((P O) O)
Norm := \n.((n s) z)

Main := (Norm T)
