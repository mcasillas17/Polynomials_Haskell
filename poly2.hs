absolute n = if n>=0 then n else -n
getSign x = if x<0 then " - " else if x>0 then " + " else ""

crop ('0':xs) = crop xs
crop (' ':xs) = crop xs
crop ('.':xs) = crop xs
crop xs = xs

deleteZeroes [x] = [x]
deleteZeroes (0:xs) = deleteZeroes xs
deleteZeroes xs = xs

printPolyAux [] = "0"
printPolyAux [x] = sign ++ coef
     where sign = getSign x
           value = absolute x
           coef = show value
printPolyAux (0:xs) = printPolyAux xs
printPolyAux (x:y:[]) = sign1 ++ c1 ++ "x " ++ sign2 ++ " " ++ mon
     where value1 = absolute x
           value2 = absolute y
           c1 = if value1/=1 then show value1 else ""
           mon = show value2
           sign1 = getSign x
           sign2 = getSign y
printPolyAux poly@(x:xs) = sign ++ coef ++ "x^" ++ deg ++ " " ++ rest
     where n = length xs
           value = absolute x
           coef = if value/=1 then show value else ""
           deg = show n
           rest = printPolyAux xs
           sign = getSign x

printPoly [] = "0"
printPoly [x] = show x
printPoly poly@(x:xs) = result
      where poly2 = reverse poly
            xs2 = deleteZeroes poly2
            y = head xs2
            ans = printPolyAux xs2
            temp = drop 3 ans
            temp2 = if y<0 then "-"++temp else temp
            temp3 = reverse temp2
            result = reverse (crop temp3)

printPolynomial poly = printPoly (reverse (deleteZeroes (reverse poly)))