decompress :: String -> String
decompress ('(':xs) = decompressRepeat xs
decompress []       = []
decompress s        = decompressRaw s

decompressRepeat :: String -> String
decompressRepeat s = 
    let left         = takeWhile (/= 'x') s
        right        = takeWhile (/= ')') . tail $ dropWhile (/= 'x') s
        len          = read left
        count        = read right
        s'           = tail $ dropWhile (/= ')') s
        repeatString = take len s'
        s''          = drop len s'
        decompressed = concat . take count $ repeat repeatString
    in decompressed ++ decompress s''
    

decompressRaw :: String -> String
decompressRaw s = 
    let left  = takeWhile (/= '(') s
        right = dropWhile (/= '(') s
    in left ++ decompress right

main = do
    l <- readFile "09input.txt"
    let str =  decompress l
    putStrLn l 
    putStrLn . show . length $ str
