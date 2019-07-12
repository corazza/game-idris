module Exception

%access public export

Checked : Type -> Type
Checked = Either String

fail : String -> Checked a
fail = Left

-- catResults : List (Checked a) -> Checked (List a)
-- catResults = catResults' [] where
--   catResults' : List a -> List (Checked a) -> Checked (List a)
--   catResults' acc [] = pure acc
--   catResults' acc (Left e :: xs) = fail e
--   catResults' acc (Right r :: xs) = catResults' (r :: acc) xs

export
catResults : List (Checked a) -> Checked (List a)
catResults = foldr toChecked (pure empty) where
  toChecked : (elem : Checked a) -> (acc : Checked (List a)) -> Checked (List a)
  toChecked (Left e) (Left es) = fail $ e ++ "\n" ++ es
  toChecked (Left e) (Right _) = fail e
  toChecked (Right as) (Left e) = fail e
  toChecked (Right x) (Right as) = pure $ x :: as
