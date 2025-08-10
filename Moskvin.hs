module Moskvin where

import Data.Char
import Data.Function

fibonacci :: Integer -> Integer
fibonacci n = helper 0 1 n
  where
    helper a b counter
      | counter == 0 = a
      | counter > 0  = helper b (a + b) (counter - 1)
      | otherwise    = helper (b - a) a (counter + 1)

seqA :: Integer -> Integer
seqA n = helper 1 2 3 n
    where helper a _ _ 0 = a
          helper a b c counter = helper b c (c + b - 2*a) (counter - 1)

-- Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = helper (abs x) 0 0
  where
    helper n digitsSum digitsCount
      | n < 10 = (digitsSum + n, digitsCount + 1)
      | otherwise = helper (n `div` 10) (digitsSum + n `mod` 10) (digitsCount + 1)

-- Реализуйте функцию, находящую значение определённого интеграла от заданной функции 
-- f на заданном интервале [a,b] методом трапеций.
-- (Используйте равномерную сетку; достаточно 1000 элементарных отрезков.)
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * (average + sum 1 0) 
  where
    n = 1000
    h = (b - a) / n
    average = (f a + f b) / 2
    sum counter acc
      | counter == n = acc
      | otherwise = sum (counter+1) (acc + f (a + counter*h))

-- Напишите реализацию функций g и h.
multSecond = g `on` h
  where g = (*)
        h (a, b) = b

-- Напишите функцию с сигнатурой...
-- вычисляющую среднее значение переданных в нее аргументов:
avg :: Int -> Int -> Int -> Double
avg a b c = (fromIntegral sum) / 3 where
  sum = toInteger a + toInteger b + toInteger c

-- Составьте список сумм соответствующих элементов трех заданных списков.
-- Длина результирующего списка должна быть равна длине самого длинного из заданных списков, при этом «закончившиеся» списки не должны давать вклада в суммы.
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 a b c = helper (pad a) (pad b) (pad c)
    where
        maxLen = length a `max` length b `max` length c
        pad x = x ++ (take (maxLen - length x) (repeat 0))
        helper [] [] [] = []
        helper (x:xs) (y:ys) (z:zs) = (x+y+z) : helper xs ys zs

{-
Напишите функцию groupElems которая группирует одинаковые элементы в списке (если они идут подряд) и возвращает список таких групп.
```haskell
GHCi> groupElems []
[]
GHCi> groupElems [1,2]
[[1],[2]]
GHCi> groupElems [1,2,2,2,4]
[[1],[2,2,2],[4]]
GHCi> groupElems [1,2,3,2,4]
[[1],[2],[3],[2],[4]]
```
-}
groupElems :: Eq a => [a] -> [[a]]
groupElems xs = groupElems' [] [] xs where
    groupElems' grouped [] []              = grouped
    groupElems' grouped [] (x:xs)          = groupElems' grouped [x] xs
    groupElems' grouped acc []             = grouped ++ [acc]
    groupElems' grouped acc@(x:_) (y:rest) = 
      if x == y
        then groupElems' grouped (x:acc) rest
        else groupElems' (grouped ++ [acc]) [y] rest

{-
Напишите функцию readDigits, принимающую строку и возвращающую пару строк.
Первый элемент пары содержит цифровой префикс исходной строки, а второй - ее оставшуюся часть.
-}
readDigits :: String -> (String, String)
readDigits = span isDigit

-- Квиксорт...
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort [x]    = [x]
qsort (x:xs) = qsort less ++ [x] ++ qsort greater
  where
    less    = filter (<= x) xs
    greater = filter (> x) xs


perms []  = [[]]
perms [x] = [[x]]
perms xs  = concatMap (\(y:ys) -> map (y:) $ perms ys) (rot xs $ length xs - 1)
  where
    rot [] _ = []
    rot xs 0 = [xs]
    rot l@(x:xs) n = l : rot (xs ++ [x]) (n-1)


-- Реализуйте функцию delAllUpper, удаляющую из текста все слова, целиком состоящие из символов в верхнем регистре.
-- Предполагается, что текст состоит только из символов алфавита и пробелов, знаки пунктуации, цифры и т.п. отсутствуют.
delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words

-- Напишите функцию max3, которой передаются три списка одинаковой длины и которая возвращает список той же длины,
-- содержащий на k-ой позиции наибольшее значение из величин на этой позиции в списках-аргументах.
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> x `max` y `max` z)

-- Реализуйте c использованием функции zipWith функцию fibStream, возвращающую бесконечный список чисел Фибоначчи.
fibStream :: [Integer]
fibStream = [0,1] ++ zipWith (+) fibStream (tail fibStream)

-- Предположим, что функция repeat, была бы определена следующим образом:
-- ```haskell
-- repeat = iterate repeatHelper
-- ```
-- определите, как должна выглядеть функция repeatHelper.
repeat x = iterate repeatHelper where
  repeatHelper = id
  