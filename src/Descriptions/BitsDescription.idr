module Descriptions.BitsDescription

import Data.Bits

import GameIO
import Exception

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

decideBits : List (Bits 16) -> Int
decideBits = fromInteger . bitsToInt . foldr or (intToBits 0)

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
