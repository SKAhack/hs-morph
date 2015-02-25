module Morph (
    toSnake,
    toSnakeCaps,
    toDashed,
    toHuman,
    toTitle,
    toCamel,
    toUpperCamel
) where

import qualified Data.Char as C
import qualified Re as Re

toSnake :: String -> String
toSnake s = do
    let s1 = Re.replace "([A-Z\\d])([A-Z][a-z\\d])" s "\\1_\\2"
    let s2 = Re.replace "([a-z])([2-Z])" s1 "\\1_\\2"
    let s3 = Re.replace "[-. ]" s2 "_"
    map C.toLower s3

toSnakeCaps :: String -> String
toSnakeCaps s = map C.toUpper $ toSnake s

toDashed :: String -> String
toDashed s = do
    let s1 = Re.replace "([A-Z\\d])([A-Z][a-z\\d])" s "\\1-\\2"
    let s2 = Re.replace "([a-z])([2-Z])" s1 "\\1-\\2"
    let s3 = Re.replace "[_. ]" s2 "-"
    map C.toLower s3

toHuman :: String -> String
toHuman s = do
    let s1 = Re.replace "([A-Z\\d])([A-Z][a-z\\d])" s "\\1 \\2"
    let s2 = Re.replace "([a-z])([2-Z])" s1 "\\1 \\2"
    let s3 = Re.replace "[-_.]" s2 " "
    toUpperFirstChar $ map C.toLower s3

toTitle :: String -> String
toTitle s = Re.replaceMap " ([a-z])" (toHuman s) f
    where
        f ps = " " ++ (map C.toUpper $ ps!!1)

toCamel :: String -> String
toCamel s = Re.replaceMap "_([a-z])" (toSnake s) f
    where
        f ps = map C.toUpper $ ps!!1

toUpperCamel :: String -> String
toUpperCamel s = toUpperFirstChar $ toCamel s

toUpperFirstChar :: String -> String
toUpperFirstChar s = (map C.toUpper $ take 1 s) ++ (drop 1 s)
