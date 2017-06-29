module ModuleOne where
import Data.Char

-- вычисляет длину трехмерного вектора
lenVec3 x y z = sqrt (x^2 + y^2 + z^2)

-- возвращает знак числа (1, -1, 0)
sign x = if x > 0 then 1 else (if x < 0 then -1 else 0)

infixl 6 *+*
a *+* b = a^2 + b^2

-- возвращает модуль разности переданных ему аргументов
infix 6 |-|
x |-| y = max x y - min x y

-- тип частично примененной standardDiscount
discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

-- принимает два символа и возвращает число, составленное из этих символов,
-- если оба символа числовые, и 100 в противном случае
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100

-- возвращает расстояние между двумя точками
dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p1 - fst p2)^2 + (snd p1 - snd p2)^2

-- вычисляет двойной факториал
-- Предполагается, что аргумент функции может принимать только неотрицательные значения
doubleFact :: Integer -> Integer
doubleFact (-1) = 1
doubleFact 0 = 1
doubleFact n = n * doubleFact (n - 2)

-- Измените определение функции fibonacci так, чтобы она была определена
-- для всех целых чисел и порождала при отрицательных аргументах указанную последовательность
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

-- более эффективная реализация, имеющую линейную сложность (по числу рекурсивных вызовов)
-- при помощи механизма аккумуляторов
fib :: Integer -> Integer
fib n = helper 0 1 n
helper x y 0 = x
helper x y n | n > 0 = helper y (x + y) (n - 1)
             | n < 0 = helper y (x - y) (n + 1)

-- функция seqA, находящая элементы следующей рекуррентной последовательности
-- a0 = 1; a1 = 2; a2 = 3; a(k + 3) = a(k + 2) + a(k + 1) − 2ak.
seqA :: Integer -> Integer
seqA n 
     | n >= 0 = let
        helper x y z 0 = x
        helper x y z n = helper y z (z + y - 2 * x) (n - 1)
    in helper 1 2 3 n
    | n < 0 = error "Negative arg" 

-- функция, находящая сумму и количество цифр десятичной записи заданного целого числа
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = helper (abs x) 0 0
    where 
        helper x sumx cnt
            | x > 9 = helper (div x 10) (sumx + mod x 10) (cnt + 1)
            | otherwise = (sumx + mod x 10, cnt + 1)

-- функция, находящая значение определённого интеграла от заданной функции f
-- на заданном интервале [a,b] методом трапеций
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = ((f a + f b) / 2 + helper f a b 999 0) * ((b - a) / 1000)
    where
        helper f a b 0 sumx = sumx
        helper f a b n sumx = helper f a b (n - 1) (sumx + f (a + n * ((b - a) / 1000)))

   
-- **************** начало: лекционный материал ***************
factorial n = if n == 0 then 1 else n * factorial (n - 1)

factorial' 0 = 1
factorial' n = n * factorial (n - 1)

factorial'' 0 = 1
factorial'' n = if n < 0 then error "arg must be >= 0" else n * factorial (n - 1)

factorial3 0 = 1
factorial3 n | n < 0 = error "arg must be >= 0"
             | n > 0 = n * factorial (n - 1)

factorial4 n | n == 0    = 1
             | n > 0     = n * factorial (n - 1)
             | otherwise = error "arg must be >= 0"

factl (-1) = 1
factl 0 = 1
factl n = if n < 0 then error "negative n" else n * factl (n - 2)

factorial5 n | n >= 0 = helper1 1 n
             | otherwise = error "Nagative argument"
helper1 acc 0 = acc
helper1 acc n = helper1 (acc * n) (n - 1)

roots :: Double -> Double -> Double 
         -> (Double, Double)

roots a b c = 
    (
        (-b + sqrt(b ^ 2 - 4 * a * c)) / (2 * a)
    ,
        (-b - sqrt(b ^ 2 - 4 * a * c)) / (2 * a)
    )

roots' a b c =
    let d = sqrt(b ^ 2 - 4 * a * c) in 
    ((-b + d) / (2 * a), (-b - d) / (2 * a))

rootsDif a b c = let
    (x1, x2) = roots a b c
    in x2 - x1
-- **************** конец: лекционный материал ***************