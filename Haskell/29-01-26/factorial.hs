-- Factorial en Haskell (recursiva clásica)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
  print (factorial 5)
