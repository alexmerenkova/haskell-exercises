module ModuleTwo where
import Data.Function
import Data.Tuple
import Data.Char hiding (toLower)
import Data.List (union)
import qualified Data.Set as Set

-- параметрический полиморфизм
-- полностью игнорирует первый и третий аргумент, а возвращает второй
getSecondFrom :: t1 -> t2 -> t3 -> t2
getSecondFrom a b c = b

-- multSecond перемножающая вторые элементы пар, реализована следующим образом
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- on op f x y = f x `op` f y
-- реализовать g и h
multSecond = g `on` h
g x y = x * y
h x = snd x

-- функция on3, имеющая семантику, схожую с on, но принимающая в качестве
-- первого аргумента трехместную функцию
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

-- функция одной переменной doItYourself выбирает наибольшее из переданного
-- ей аргумента и числа 42, затем возводит результат выбора в куб и, наконец,
-- вычисляет логарифм по основанию 2 от полученного числа.
-- написать реализации функций fe, ge и he в бесточечном стиле.
-- max(x, 42) -> res ^ 3 - > logBase 2 res 
doItYourself = fe . ge . he
fe = logBase 2
ge = (^ 3)
he = max 42

-- в модуле Data.Tuple стандартной библиотеки определена функция swap :: (a,b) -> (b,a),
-- переставляющая местами элементы пары
-- эта функция может быть выражена в виде: swap = f (g h)
swapx = f1 (g1 h1)
f1 = uncurry
g1 = flip
h1 = (,)

-- класс типов Printable, предоставляющий один метод toString — функцию одной переменной,
-- которая преобразует значение типа, являющегося представителем Printable,
-- в строковое представление.
-- типы данных Bool и () сделать представителями этого класса типов
class Printable a where
    toString :: a -> [Char]

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString () = "unit type"

-- сделать тип пары представителем класса типов Printable
instance (Printable a, Printable b) => Printable (a, b) where
    toString p = "(" ++ toString (fst p) ++ "," ++ toString (snd p) ++ ")"

-- задать реализацию по умолчанию метода stompOrStab, которая вызывает метод stomp,
-- если переданное ему значение приводит в ярость Морка; вызывает stab, если оно
-- приводит в ярость Горка и вызывает сначала stab, а потом stomp, если оно приводит в ярость их обоих.
-- Если не происходит ничего из вышеперечисленного, метод должен возвращать переданный ему аргумент
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | doesEnrageMork a && doesEnrageGork a = stomp (stab a)
                  | doesEnrageMork a = stab a
                  | doesEnrageGork a = stomp a
                  | otherwise = a

-- ip address (fun)
a = 127.2
b = 24.1
c = 20.1
d = 2
ip = show a ++ show b ++ show c ++ show d

-- реализовать класс типов, где succ и pred обеспечивают циклическое поведение
class (Eq a, Enum a, Bounded a) => SafeEnum a where

    ssucc :: a -> a
    ssucc x | x == maxBound = minBound 
            | otherwise = succ x

    spred :: a -> a
    spred x | x == minBound = maxBound
            | otherwise = pred x 

instance SafeEnum Bool

-- функция, вычисляющая среднее значение переданных в нее аргументов
avg :: Int -> Int -> Int -> Double
avg x y z =  (fromIntegral x + fromIntegral y + fromIntegral z) / 3.0

-- **************** начало: лекционный материал ***************

fun x = x * 2 + 7
fun' = \x -> x * 2 + 7 

lenVect x y = sqrt $ x^2 + y^2
lenVect' = \x -> \y -> sqrt $ x^2 + y^2
lenVect'' = \x  y -> sqrt $ x^2 + y^2

sumFstFst = (+) `on` helper
    where helper pp  = fst $ fst pp

sumFstFst' = (+) `on` (\pp -> fst $ fst pp)

sumFstFst'' = (+) `on` (fst . fst)

mySum acc 0 = acc
mySum (result, ()) n = (mySum $! (result + n, ())) $ n - 1

goSum = mySum (0, ())
-- **************** конец: лекционный материал ***************