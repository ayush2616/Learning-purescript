module Factor where

import Data.Array
import Prelude

import Control.Monad.Eff.Console (log)
import Control.MonadZero (guard)
import DOM.HTML (window)
import Data.Foldable (product)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (fst, snd)



factors :: Int -> Array (Array Int)
factors n =filter (\xs ->product  xs==n) $ do
  i <- 2..n
  j <- i..n
  pure [i,j]

func1 []=[]
func1 [x]=[x]
func1 [x,y]=[x+y]
func1 arr= cons ((fromMaybe 0 (head arr) )+(fromMaybe 0 (head (fromMaybe [] (tail arr))))) (func1 (fromMaybe [] (tail arr)))

isPrime :: Int -> Boolean
isPrime n=(length $factors n)==0
carProd arr1 arr2 = do
  i<-arr1
  j<-arr2
  [i*j]


getSum arr=map (\p -> fst p + snd p) $ arr `zip` (fromMaybe [] $ tail arr)


fact n | n==0 = 1
       | otherwise = n * fact (n-1)
triple n = do
  i<-1..n
  guard (i==3)
  j<-i..n
  k<-j..n
  [[i,j,k]]


reverse1 :: forall a. Array a -> Array a
reverse1 = foldr (\x xs -> [x] <> xs) []

factorization :: Int -> Int -> Array Int
factorization _ 1=[]
factorization i n|i==n =[i]
factorization 1 n=factorization 2 n
factorization i n=
  if (mod n i)==0
    then cons i (factorization i (n/i))
    else factorization (i+1) n
