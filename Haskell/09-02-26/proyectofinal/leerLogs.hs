import Data.List (nub, intercalate)
import System.Environment (getArgs)

data LogEntry = LogEntry {
    ip :: String,
    path :: String,
    code :: Int
} deriving (Show)

parseLog :: String -> LogEntry
parseLog line =
    let parts = words line
    in LogEntry (head parts) (parts !! 1) (read (parts !! 2))

analyzeLogs :: [LogEntry] -> (Int, Int, [String])
analyzeLogs logs = (totalRequests, errorCount, uniqueIPs)
    where
        totalRequests = length logs
        errorCount = length (filter (\entry -> code entry >= 400) logs)
        uniqueIPs = nub (map ip logs)

-- Convertir una lista de strings a JSON array
toJsonArray :: [String] -> String
toJsonArray xs = "[" ++ intercalate ", " (map (\s -> "\"" ++ s ++ "\"") xs) ++ "]"

-- Generar JSON de salida
toJson :: Int -> Int -> [String] -> String
toJson total errors ips = concat
    [ "{\n"
    , "  \"total_peticiones\": ", show total, ",\n"
    , "  \"errores\": ", show errors, ",\n"
    , "  \"ips_unicas\": ", toJsonArray ips, "\n"
    , "}"
    ]

main :: IO ()
main = do
    args <- getArgs
    let filePath = case args of
            (f:_) -> f
            []    -> "server.log"

    -- 1. Leer el archivo .log
    content <- readFile filePath

    -- 2. Parsear cada linea
    let logs = map parseLog (lines content)

    -- 3. Analizar
    let (total, errors, ips) = analyzeLogs logs

    -- 4. Imprimir como JSON
    putStrLn (toJson total errors ips)
