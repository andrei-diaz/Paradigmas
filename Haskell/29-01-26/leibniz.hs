-- Fórmula de Leibniz para aproximar π
-- π/4 = 1 - 1/3 + 1/5 - 1/7 + 1/9 - ...

leibniz :: Int -> Double
leibniz n = 4 * sum [(-1) ^ k / fromIntegral (2 * k + 1) | k <- [0 .. n]]

main :: IO ()
main = do
  print (leibniz 1000)
