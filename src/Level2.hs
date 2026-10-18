{-# OPTIONS_GHC -Wno-orphans #-}
-- | Домашка 6. Задачи к сессии. Ord дерева из уровня 1 — учебный orphan.
module Level2 where

import Data.Int (Int8)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Level1 (IntTree (..), StructEq (..))
import MetaUtils (todo, todoImpl)
import Text.Read (readMaybe)


-- 2.1. Порядок деревьев, согласованный с Eq из 1.2.
-- IntLeaf равен IntLeaf и меньше любого IntNode.
-- Узлы сравниваются лексикографически: левое поддерево, ВТОРАЯ компонента
-- пары, правое поддерево. Первая компонента пары по-прежнему игнорируется.
-- Выберите минимально необходимый набор методов. Законы Ord — лекция 6.

instance Ord IntTree where
  compare a b = case (a, b) of
    (IntLeaf, IntLeaf) -> EQ
    (IntLeaf, _) -> LT
    (_, IntLeaf) -> GT
    (IntNode l (_, p) r, IntNode l' (_, p') r') ->
      case compare l l' of
        EQ -> case compare p p' of
          EQ -> compare r r'
          other -> other
        other -> other


-- 2.2. Циклический перебор.
-- Реализуйте класс типов CycleEnum. Обе его функции cycSucc и cycPred
-- ведут себя как succ и pred, но при достижении максимального и минимального
-- значений соответственно начинают сначала (то есть с минимального
-- и максимального соответственно).
-- Сделайте Bool представителем CycleEnum. Сделайте Int8 представителем CycleEnum.

class CycleEnum a where
  cycSucc :: a -> a
  cycPred :: a -> a

instance CycleEnum Bool where
  cycSucc = not
  cycPred = not

instance CycleEnum Int8 where
  cycSucc a = if (a == maxBound) then minBound else a + 1
  cycPred a = if (a == minBound) then maxBound else a - 1

-- 2.3. Обёртка для циклического перебора.
-- Для newtype-обёртки Cycling реализуйте CycleEnum при условии, что оборачиваемый
-- тип удовлетворяет минимальному набору ограничений.

newtype Cycling a = Cycling a
  deriving newtype (Eq, Show, Bounded, Enum)

instance (Eq a, Bounded a, Enum a) => CycleEnum (Cycling a) where
  cycSucc a = if (a == maxBound) then minBound else succ a
  cycPred :: Enum a => Cycling a -> Cycling a
  cycPred a = if (a == minBound) then maxBound else pred a


-- 2.4. Печать строк матрицы.
-- Matrix хранит список непустых строк; равенство длин строк НЕ требуется.
-- Для пустого списка выведите "EMPTY". Иначе каждую строку покажите как
-- обычный список, разделяя строки одним '\n', без завершающего '\n'.
-- Пример: Matrix [1 :| [2], 3 :| [4]] -> "[1,2]\n[3,4]".
-- Напишите инстанс Show целиком, с необходимыми ограничениями.
-- Можно использовать NE.toList и List.intercalate.

newtype Matrix a = Matrix [NonEmpty a]
  deriving Eq

instance (Show a) => Show (Matrix a) where
  show matrix = case matrix of
    Matrix [] -> "EMPTY"
    Matrix (xs : []) -> show (NE.toList xs)
    Matrix (xs : xxs) -> show (NE.toList xs) ++ "\n" ++ show (Matrix xxs)


-- 2.5. Разберите показанную матрицу.
-- Для "EMPTY" верните Just (Matrix []). В остальных случаях каждая строка
-- входа должна целиком читаться как непустой список через readMaybe.
-- Если хотя бы одна строка не читается или пуста ([]), верните Nothing.
-- Пустая строка входа тоже ошибочна. Перевод строки в конце входа разрешён.
-- Закон: parseMatrix (show m) == Just m (для Int и Char).
-- Допишите parseRow; обвязка уже дана. Вспомните решение 1.3 из ДЗ 5.

parseMatrix :: Read a => String -> Maybe (Matrix a)
parseMatrix raw = case lines raw of
  [] -> Nothing
  ["EMPTY"] -> Just (Matrix [])
  rows -> fmap Matrix (parseRows rows)
  where
    parseRows :: Read b => [String] -> Maybe [NonEmpty b]
    parseRows [] = Just []
    parseRows (row : rest) = case parseRow row of
      Nothing -> Nothing
      Just parsedRow -> case parseRows rest of
        Nothing -> Nothing
        Just parsedRest -> Just (parsedRow : parsedRest)

    parseRow :: Read b => String -> Maybe (NonEmpty b)
    parseRow s = case readMaybe s :: Read b => Maybe [b] of
      Nothing -> Nothing
      Just [] -> Nothing
      Just (x:xs) -> Just (x :| xs)


-- 2.6. Сравнение структуры пары.
-- Сделайте пару представителем StructEq из задачи 1.4.
-- Для пары (e, a) форма включает фиксированную первую компоненту e:
-- сравните её через Eq, а второй компонентой пренебрегите.
-- Напишите инстанс целиком, выбрав необходимые ограничения.

instance Eq e => StructEq ((,) e) where
  (==#) (e, _) (e', _) = e == e'


-- 2.7. Функтор внутри списка, функции и пары.
-- Преобразуется только a. Длина и порядок списка, окружение e и метка p
-- сохраняются. Напишите инстанс целиком, используя
-- стандартные инстансы этих структур.
-- deriving Functor вместо собственного инстанса использовать нельзя.
-- Подсказка: (->) e действует посткомпозицией, (,) p меняет второй компонент.

newtype PairReaderList e p a = PairReaderList
  { getPairReaderList :: [e -> (p, a)] }

instance Functor (PairReaderList e p) where
  fmap f prl = case prl of
    PairReaderList list ->
      PairReaderList
        ( map
            ( \g e -> case (g e) of
                (p, a) -> (p, f a) )
            list )
