doubleMe x = x + x

superDoubleMe x = doubleMe x + doubleMe x

doubleUs x y = x * 2 + y * 2

-- dobleUs' x y = dobleMe x + dobleMe y

doubleSmallNumber x =
  if x > 100
    then x
    else x + 2
