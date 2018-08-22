-- function that checks the sign of a number and returns a string
getSign x = if x<0 then " - " else if x>0 then " + " else ""
-- function that crops all white spaces from the beginning of a string
crop (' ':xs) = crop xs
crop ('.':xs) = crop xs
crop xs = xs
-- function that deletes all starting zeroes from a reversed polynomial representation 
deleteZeroes [x] = [x]
deleteZeroes (0:xs) = deleteZeroes xs
deleteZeroes xs = xs
-- auxiliar function that prints a polynomial using the format "a_n*x^n + a_n-1*x^n-1 + ... + a_0"
printPolyAux [] = ""
printPolyAux [0] = ""
printPolyAux [x] = getSign(x) ++ show(abs x)
printPolyAux (0:xs) = printPolyAux xs
printPolyAux (x:y:[]) = getSign(x) ++ (if value1/=1 then show value1 else "") ++ "x " ++ getSign(y) ++ " " ++ show(value2)
     where value1 = abs x
           value2 = abs y
printPolyAux poly@(x:xs) = getSign(x) ++ (if value/=1 then show value else "") ++ "x^" ++ show(length xs) ++ " " ++ printPolyAux(xs)
     where value = abs x
--- main function to get the show instance of the polynomial, this crops the starting sign if it is positive
printPoly [] = "0"
printPoly [x] = show x
printPoly poly@(x:xs) = result
      where xs2 = deleteZeroes (reverse poly)
            y = head xs2
            temp = drop 3 (printPolyAux xs2)
            temp2 = if y<0 then "-"++temp else temp
            result = reverse (crop (reverse temp2))
--- main call for the print function
printPolynomial poly = printPoly . reverse . deleteZeroes $ reverse poly
-- function that evaluates a polynomial, receives the reversed notation for the polynomial
evaluateAux [] _ = 0
evaluateAux [y] _ = y
evaluateAux (y:ys) x = y*x^(length ys) + (evaluateAux ys x)
-- main call for the polynomial evaluation, reverses the polynomial and uses tha auxiliar method
-- also, this receives the value for the variable x in the polynomial
evaluatePolynomial ys x = evaluateAux (reverse ys) x
-- function that adds two polynomials (note that it is not reversed)
addPolynomials [] [] = []
addPolynomials [] ys = ys
addPolynomials xs [] = xs
addPolynomials (x:xs) (y:ys) = x+y : addPolynomials xs ys
-- function that negates a polynomial, useful for the subtraction
negatePolynomial [] = []
negatePolynomial (x:xs) =  -x : negatePolynomial xs
-- function that subtracts ys from xs
subtractPolynomials [] [] = []
subtractPolynomials xs ys = addPolynomials xs (negatePolynomial ys)
-- function that calculates the derivative of a Polynomial in reversed notation and without leading zeroes
differentiateAux [] = [0]
differentiateAux [x] = [0]
differentiateAux (x:y:[]) = [x]
differentiateAux (x:xs) = x*(length xs):differentiateAux xs
-- main call to the differentiation, receives the not reversed notation of a polynomial
-- deletes all leading zeroes and calculates the derivative
differentiatePolynomial xs = reverse . differentiateAux . deleteZeroes $ reverse xs
-- function that takes a polynomial, a coefficient and an exponent (in the form a*x^n)
-- i.e. a monomial, and multiplies the polynomial by that monomial
multiplyByAXN [] a n = []
multiplyByAXN xs a n = (replicate n 0) ++ [x*a | x<-xs]
-- function that creates a list of all the partial results of multiplying every monomial in p
-- against the polynomial q. Note that the polynomial p is reversed in order to get exponent of X
multiplyAux [] _ = []
multiplyAux p@(x:xs) q@(y:ys) = multiplyByAXN q x (length xs) : ( multiplyAux xs q )
-- function that multiplies two polynomials, here we use an advanced Haskell function called foldl that iterates
-- in a list from left to right, a lambda function (to operate in every element of the list),
-- an accumulator to store intermediate results an the one that is returned, and the list we iterate in.
-- Now let's check what we do... 
-- The list we are gonna iterate in is the one that contains every polynomial resulting of multiplying all the monomials
-- in p against the polynomial q, and we need to add every polynomial in this list to get the resulting product
-- The lambda function we are using receives the accumulator "acc" (initally the empty list [])
-- representing the polynomial zero, this is passed before the list we iterate in.
-- The lambda function takes the acumulutator "acc", the next element in the list and applies the addPolynomials function
-- two both parameters, then stores the result in "acc".
-- At the end of the foldl application, "acc" has the sum of all the polynomials in the original list, so we return "acc"
multiplyPolynomial [] [] = []
multiplyPolynomial p q = foldl (\acc x -> addPolynomials acc x) [] (multiplyAux (reverse p) q)
-- function that composes p and q polynomials, using q as the variable in the polynomial q
-- apply Horner's rule to polynomial evaluation, using a polynomial instead of a single value (works for both)
-- we use a foldl to iterate in the reversed notation of the polynomial q, using the polynomial zero as the accumulator
-- we add the coefficient of the polynomial p to the product between the accumulator and the polynomial q
-- this results in the composition of both polynomials, and is stored in "acc", that is returned
composePoly _ [] = []
composePoly p@(x:xs) q@(y:ys) = foldl (\acc x -> addPolynomials [x] (multiplyPolynomial acc q)) [] (reverse p)
main = do 
    let zero = [0]
    print $ "zero(x)     = "++printPolynomial zero
    let p = [1,2,3,4]
    print $ "p(x)        = "++(printPolynomial p)
    let q = [5,0,3]
    print $ "q(x)        = "++(printPolynomial q)
    let r1 = addPolynomials p q
    print $ "p(x) + q(x) = "++(printPolynomial r1)
    let r2 = multiplyPolynomial p q
    print $ "p(x) * q(x) = "++(printPolynomial r2)
    let r3 = composePoly p q
    print $ "p(q(x))     = "++(printPolynomial r3)
    let r4 = subtractPolynomials zero  p
    print $ "0 - p(x)    = "++(printPolynomial r4)
    let r5 = evaluatePolynomial p 3
    print $ "p(3)        = "++(show r5)
    let r6 = differentiatePolynomial p
    print $ "p'(x)       = "++(printPolynomial r6)
    let r7 = differentiatePolynomial r6
    print $ "p''(x)      = "++(printPolynomial r7)