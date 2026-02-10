bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 25.0 = "You're normal weight!"
    | bmi <= 40.0 = "You're overweight"
    | otherwise   = "You're obese"
 