-- ax² + bx + c = 0

quadratic :: Double -> Double -> Double -> (Double, Double)
quadratic a b c = (x1, x2)
  where
    discriminant = b * b - 4 * a * c
    x1 = (-b + sqrt discriminant) / (2 * a)
    x2 = (-b - sqrt discriminant) / (2 * a)

main :: IO ()
main = do
  print (quadratic 1 (-5) 6)
