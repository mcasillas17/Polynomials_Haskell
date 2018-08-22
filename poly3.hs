absolute n = if n>=0 then n else -n
getSign x = if x<0 then " - " else " + "

crop ('0':xs) = crop xs
crop (' ':xs) = crop xs
crop ('.':xs) = crop xs
crop ('+':xs) = crop xs
crop xs = xs

deleteZeroes [x] = [x]
deleteZeroes (0:xs) = deleteZeroes xs
deleteZeroes xs = xs

printPolyAux [] = "0"
printPolyAux [x] = getSign(x) ++ show(absolute x)
printPolyAux (0:xs) = printPolyAux xs
printPolyAux poly@(x:xs) = getSign(x) ++ coef ++ var ++ printPolyAux(xs)
       where coef = if x/=1 then show(absolute x) else ""
             n = length xs
             var = if n>1 then "x^"++show n else "x"

printPoly [] = "0"
printPoly [0] = "0"
printPoly [x] = getSign(x) ++ " " ++ show(absolute x)
printPoly poly@(x:xs) = (if last poly<0 then "-" else "") ++ reverse(crop(reverse(noSign)))
      where noSign = drop 3 (printPolyAux(reverse poly))

printPolynomial poly = printPoly(reverse (deleteZeroes(reverse (poly))))