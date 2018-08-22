coprimes k xs =  filter (coprime k) xs
 where coprime a b = gcd a b == 1

myFunction k xs = sum $ map sum $ map (map (gcd k)) xs

main = do
       print $ myFunction 7 [[1..10],[2..15],[2..7]]