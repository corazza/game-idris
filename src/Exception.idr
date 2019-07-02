module Exception

%access public export

-- data Exception = Root String | From Exception String
--
-- Show Exception where
--   show (Root message) = message
--   show (From x message) = show x ++ "\n" ++ message

Checked : Type -> Type
Checked = Either String

fail : String -> Checked a
fail = Left

catResults : List (Checked a) -> Checked (List a)
catResults = catResults' [] where
  catResults' : List a -> List (Checked a) -> Checked (List a)
  catResults' acc [] = pure acc
  catResults' acc (Left e :: xs) = fail e
  catResults' acc (Right r :: xs) = catResults' (r :: acc) xs

hmm : Int -> Checked Int
hmm x = let x' = x - 10 in
  if x' < 0 then fail "x negative!" else pure x'

testDo : Int -> Checked String
testDo x = with Checked do
  x' <- hmm x
  pure $ "success: " ++ show x'
