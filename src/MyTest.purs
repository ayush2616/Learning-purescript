module MyTest where


import Prelude (mod, (+), (==))

import Data.Array (head, tail)
import Data.Maybe (fromMaybe)

findEven :: Int-> Boolean
findEven n=mod n 2==0

evenInArr :: Array Int ->Int
evenInArr []=0
evenInArr arr=
  if findEven (fromMaybe 111 (head arr))
  then 1+evenInArr (fromMaybe [] (tail arr))
  else evenInArr (fromMaybe [] (tail arr))
