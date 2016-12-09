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






main = readFile "09input.txt" >>= putStrLn . show . decompress2 

decompress2 :: String -> Int
decompress2 ('(':xs) = decompressRepeat2 xs
decompress2 []       = 0
decompress2 s        = decompressRaw2 s

decompressRepeat2 :: String -> Int 
decompressRepeat2 s = 
    let left         = takeWhile (/= 'x') s
        right        = takeWhile (/= ')') . tail $ dropWhile (/= 'x') s
        len          = read left
        count        = read right
        s'           = tail $ dropWhile (/= ')') s
        repeatString = take len s'
        s''          = drop len s'
    in (count * (decompress2 repeatString))  + (decompress2 s'')
    

decompressRaw2 :: String -> Int 
decompressRaw2 s = 
    let left  = takeWhile (/= '(') s
        right = dropWhile (/= '(') s
    in length left + decompress2 right
   
