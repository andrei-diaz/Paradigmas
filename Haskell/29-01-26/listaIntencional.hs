-- Lista por comprensión con múltiples generadores

noums = ["rana", "zebra", "cabra"]

adjectives = ["perezoso", "enfadada", "intrigante"]

frases = [noum ++ " es " ++ adjective | noum <- noums, adjective <- adjectives]

main = print frases

-- Resultado:
-- ["rana es perezoso","rana es enfadada","rana es intrigante",
--  "zebra es perezoso","zebra es enfadada","zebra es intrigante",
--  "cabra es perezoso","cabra es enfadada","cabra es intrigante"]