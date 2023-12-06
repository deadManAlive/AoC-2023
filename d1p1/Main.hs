import Data.Char (isDigit)

digitfilter = map (read . (\x -> head x : [last x]) . filter isDigit)

main = readFile "input.txt" >>= print . sum . digitfilter . lines