import Data.List (inits, tails)
import Data.Maybe (catMaybes, mapMaybe)
import Text.Read (readMaybe)

parseStr = filter (< 10) . mapMaybe aux . concatMap (tail . inits) . tails
  where
    aux s =
      let res = readMaybe s
       in case res of
            Just n -> res
            Nothing -> case s of
              "one" -> Just 1
              "two" -> Just 2
              "three" -> Just 3
              "four" -> Just 4
              "five" -> Just 5
              "six" -> Just 6
              "seven" -> Just 7
              "eight" -> Just 8
              "nine" -> Just 9
              _ -> Nothing

pick l = 10 * head l + last l

main = readFile "../d1p1/input.txt" >>= print . sum . map (pick . parseStr) . lines