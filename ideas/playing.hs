
(.>) = \x f -> f x
infix 9 .>

tak :: [a] -> Int -> [a]
tak _ 0 = []
tak (a:as) n = a : (as.>tak)(n-1)

main = print $ tak "hello world!" 5
