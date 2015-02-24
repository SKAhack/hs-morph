# Morph for haskell

```hs
import Morph

main = do
  putStrLn $ toSnake "foo-bar-buzz"      -- foo_bar_buzz
  putStrLn $ toSnakeCaps "foo bar buzz"  -- FOO_BAR_BUZZ
  putStrLn $ toSnakeCaps "foo bar buzz"  -- FOO_BAR_BUZZ
  putStrLn $ toDashed "foo_bar_buzz"     -- foo-bar-buzz
  putStrLn $ toHuman "fooBarBuzz"        -- Foo bar buzz
  putStrLn $ toTitle "fooBarBuzz"        -- Foo Bar Buzz
  putStrLn $ toCamel "foo_bar_buzz"      -- fooBarBuzz
  putStrLn $ toUpperCamel "foo-bar-buzz" -- FooBarBuzz
```
