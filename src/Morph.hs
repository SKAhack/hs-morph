module Morph (
    toSnake,
    toSnakeCaps,
    toDashed,
    toHuman,
    toTitle,
    toCamel,
    toUpperCamel
) where

import qualified Text.Regex as R
import qualified Text.Regex.Base as B
import qualified Data.Char as C
import Data.Array ((!))

toSnake :: String -> String
toSnake s = do
    let s1 = R.subRegex (R.mkRegex "([A-Z\\d])([A-Z][a-z\\d])") s "\\1_\\2"
    let s2 = R.subRegex (R.mkRegex "([a-z])([2-Z])") s1 "\\1_\\2"
    let s3 = R.subRegex (R.mkRegex "[-. ]") s2 "_"
    map C.toLower s3

toSnakeCaps :: String -> String
toSnakeCaps s = map C.toUpper $ toSnake s

toDashed :: String -> String
toDashed s = do
    let s1 = R.subRegex (R.mkRegex "([A-Z\\d])([A-Z][a-z\\d])") s "\\1-\\2"
    let s2 = R.subRegex (R.mkRegex "([a-z])([2-Z])") s1 "\\1-\\2"
    let s3 = R.subRegex (R.mkRegex "[_. ]") s2 "-"
    map C.toLower s3

toHuman :: String -> String
toHuman s = do
    let s1 = R.subRegex (R.mkRegex "([A-Z\\d])([A-Z][a-z\\d])") s "\\1 \\2"
    let s2 = R.subRegex (R.mkRegex "([a-z])([2-Z])") s1 "\\1 \\2"
    let s3 = R.subRegex (R.mkRegex "[-_.]") s2 " "
    toUpperFirstChar $ map C.toLower s3

toTitle :: String -> String
toTitle s = do
    let s1 = toHuman s
    let matches = B.matchAllText (R.mkRegex " ([a-z])") s1
    let splited = split 0 s1 (map (\m -> m!0) matches)
    concat $ replacer splited matches
    where
        split _ s [] = [s]
        split i s m  = do
            let (offset, len) = snd (head m)
            let pre = take (offset - i) s
            let left = drop (offset + len - i) s
            pre : ( split (offset + len) left (tail m) )

        replacer [] _ = []
        replacer s [] = s
        replacer ss ms = do
            let p1 = fst ((head ms)!1)
            [head ss] ++ [" "] ++ [map C.toUpper p1] ++ (replacer (tail ss) (tail ms))

toCamel :: String -> String
toCamel s = do
    let s1 = toSnake s
    let matches = B.matchAllText (R.mkRegex "_([a-z])") s1
    let splited = split 0 s1 (map (\m -> m!0) matches)
    concat $ replacer splited matches
    where
        split _ s [] = [s]
        split i s m  = do
            let (offset, len) = snd (head m)
            let pre = take (offset - i) s
            let left = drop (offset + len - i) s
            pre : ( split (offset + len) left (tail m) )

        replacer [] _ = []
        replacer s [] = s
        replacer ss ms = do
            let p1 = fst ((head ms)!1)
            [head ss] ++ [map C.toUpper p1] ++ (replacer (tail ss) (tail ms))

toUpperCamel :: String -> String
toUpperCamel s = toUpperFirstChar $ toCamel s

toUpperFirstChar :: String -> String
toUpperFirstChar s = (map C.toUpper $ take 1 s) ++ (drop 1 s)
