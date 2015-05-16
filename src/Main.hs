module Main where

import Prelude hiding (lookup, log, abs)
import qualified Data.Map.Strict as M

import Control.Monad.Writer

type Map = M.Map
fromJust _ (Just x) = x
fromJust k _ = error $ "fromJust: " ++ show k
lookup k = fromJust k . M.lookup k
put = M.insert

-- absolute
newtype A = A Int
  deriving (Show, Eq, Ord)
-- relative
newtype R = R Int
  deriving (Show, Eq, Ord)
-- location
data L = LA A | LR R
  deriving (Show, Eq, Ord)

instance Num L where
  fromInteger = LR . R . fromIntegral
  (LR (R a)) + (LR (R b)) = LR (R (a + b))
  (LR (R a)) - (LR (R b)) = LR (R (a - b))


type Con = Map A Cell

-- a,b -> c
data Cell
  = Add L L L
  | Sub L L L
  | Mov L L
  | Jump L
  | JLeq L L L
  | JZ L L
  -- byte
  | B Int
  | Halt
  deriving (Show, Eq, Ord)

type S = (Con, A)

sep a b = show a ++ ", " ++ show b

value c ip l1 | B v1 <- lookup (relative ip l1) c
              = v1
value _ _ l1 = error $ "not a value: " ++ show l1
values c ip l1 l2 | B v1 <- lookup (relative ip l1) c
                  , B v2 <- lookup (relative ip l2) c
                  = (v1, v2)
values _ _ l1 l2 = error $ "not both values: " ++ sep l1 l2


relative :: A -> L -> A
relative _ (LA l) = l
relative (A a) (LR (R o)) = A (a + o)

incr x = relative x 1

step :: S -> Maybe S
step (context, ip) =
  case lookup ip context of
    Add al bl dest | (a, b) <- values context ip al bl
                   -> return (put (relative ip dest) (B (a + b)) context, incr ip)
    Sub al bl dest | (a, b) <- values context ip al bl
                   -> return (put (relative ip dest) (B (a - b)) context, incr ip)
    Mov al bl | v <- value context ip al ->
      return (put (relative ip bl) (B v) context, incr ip)
    Jump j -> return (context, relative ip j)
    JZ al j | a <- value context ip al ->
      if a == 0 then return (context, relative ip j)
                else return (context, incr ip)
    JLeq al bl j
      | (a, b) <- values context ip al bl
      -> if a < b then return (context, relative ip j)
                  else return (context, incr ip)
    B _ -> error $
      "\ntried to exec byte: " ++ show ip ++ "\ndump:\n" ++ show context
    Halt -> Nothing

run :: S -> S
run a | Just a' <- step a = run a'
run a = a

toCon :: A -> [Cell] -> S
toCon l m = (M.fromList . zip (map A [0..]) $ m, l)

-- syntax?
log :: Cell -> Writer [Cell] ()
log a = tell [a]
b :: Int -> Writer [Cell] ()
b = log . B
add a b c = log (Add a b c)
sub a b c = log (Sub a b c)
jump l = log (Jump l)
jz a l = log (JZ a l)
jleq a b l = log (JLeq a b l)
halt = log Halt
mov a b = log (Mov a b)

runW l = toCon l . snd . runWriter


p1 = toCon (A 2)
  [ B 22
  , B 3
  , Add (-1) (-2) 0
  , Halt
  ]
p2 = toCon (A 0)
  [ Add 2 3 0
  , Jump 3
  , B 3
  , B 5
  , Halt
  ]

-- same as p2
p2' = runW (A 0) $ do
  add 2 3 0
  jump 3
  b 3
  b 5
  halt

abs = LA . A

-- fib
-- set (A 1) as desired
psum = runW (A 5) $ do
  b (-1)
  b 222
  b 0
  b 1
  b 0
  jz (-4) 6
  add (-4) (-3) (-2)                   -- calculate
  mov (-4) (-5)                        -- shift
  mov (-4) (-5)                        -- shift
  add (abs 0) (abs 1) (abs 1) -- decr
  jump (-5)
  halt


