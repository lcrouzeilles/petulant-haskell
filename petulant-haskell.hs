import Data.Char

castellano = [12.52, 1.42, 4.67, 5.85, 13.67, 0.67, 1.01, 0.70, 6.24, 0.44, 0.01, 4.96, 3.15, 6.70, 8.67, 2.51, 0.88, 6.86, 7.97, 4.62, 3.92, 0.90, 0.02, 0.22, 0.90, 0.52]::[Float]

esMin :: Char -> Bool
esMin c = ord c >= 0x61 && ord c <= 0x7a

letANat :: Char -> Integer
letANat c | esMin c = toInteger((ord c)-0x61)
		  | otherwise = undefined

natALet :: Integer -> Char
natALet n | n >= 0 && n <= 25 = chr (fromIntegral n + 0x61)
		  | otherwise = natALet (mod n 26)

desplazar :: Integer -> Char -> Char
desplazar n c | esMin (natALet n) && esMin c = natALet (mod ((letANat c)+n) 26)
			  | otherwise = c

cantMinusc :: String -> Integer
cantMinusc [] = 0
cantMinusc (x:xs) | esMin x = 1 + cantMinusc xs
				  | otherwise = cantMinusc xs

contar :: Char -> String -> Integer
contar c [] = 0
contar c (x:xs) | c == x = 1 + contar c xs
				| otherwise = contar c xs

codificar :: Integer -> String -> String
codificar n [] = []
codificar n (x:xs) = desplazar n x : codificar n xs

decodificar n xs = codificar (26-n) xs

frec  :: String -> [Float]
frec xs = map (\x -> 100 * (fromIntegral (contar x xs))/fromIntegral (cantMinusc xs)) ['a'..'z']

rotar :: Integer -> [a] -> [a]
rotar n xs = take (length xs - fromInteger n) (drop (fromInteger n) xs) ++ take (fromInteger n) xs

chi2 :: [Float] -> [Float] -> Float
chi2 (x:xs) (y:ys) | length xs == 0 || length ys == 0 = 0
				   | otherwise = (x-y)^2/y + chi2 xs ys

index :: Float -> [Float] -> Integer
index x xs | length xs == 0 || head xs == x = 0
		   | otherwise = 1 + (index x (tail xs))

descifrar :: String -> String
descifrar cifrado = decodificar (index (minimum distancias) distancias) cifrado where distancias = map (\x -> chi2 x castellano) (map (\x -> rotar x (frec cifrado)) [0..25])
