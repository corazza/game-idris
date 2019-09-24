module Descriptions.BitsDescription

import Data.Bits

import GameIO
import Exception
import Serializer

entryToBits : (String, Int) -> (String, Bits 16)
entryToBits (name, x) = (name, intToBits $ cast x)

categoryNums : Dict String (Bits 16)
categoryNums = fromList $ map entryToBits [
  ("wall", 1),
  ("inanimate", 2),
  ("animate", 4),
  ("projectile", 8),
  ("drop", 16)
]

categoryNumsInverse : Dict (Bits 16) String
categoryNumsInverse = fromList $ map inverse $ toList categoryNums where
  inverse : (a, b) -> (b, a)
  inverse (x, y) = (y, x)

decideBits : List (Bits 16) -> Int
decideBits = fromInteger . bitsToInt . foldr or (intToBits 0)

decideCats : Int -> List (Bits 16)
decideCats x = decideCats' x Z [] where
  decideCats' : Int -> (pow' : Nat) -> (acc : List (Bits 16)) -> List (Bits 16)
  decideCats' 0 pow' acc = acc
  decideCats' x pow' acc = let forAcc = decideCats' (x `div` 2) (S pow')
      in if x `mod` 2 == 0
            then forAcc acc
            else let bit = intToBits $ pow 2 pow'
                     in forAcc (append bit acc)

export
getBits : List String -> Checked Int
getBits categories = with Checked do
    nums <- catResults $ map (flip (pick "bits") categoryNums) categories
    pure $ decideBits nums

export
getBitsMaybe : (key : String) -> JSONDict -> Checked (Maybe Int)
getBitsMaybe key dict = case hasKey key dict of
  False => pure Nothing
  True => getStrings key dict >>= getBits >>= pure . Just

export
addBits : (object : Var) ->
          (key : String) ->
          (value : Int) ->
          ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addBits object key value
  = let catBits = decideCats value
        attempts = map (flip (pick "categories") categoryNumsInverse) catBits
        strings = fromMaybe [] $ eitherToMaybe $ catResults $ attempts
        in addStringArray object key strings

export
addBitsMaybe : (object : Var) ->
               (key : String) ->
               (value : Maybe Int) ->
               ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addBitsMaybe object key (Just x) = addBits object key x
addBitsMaybe object key Nothing = pure ()
