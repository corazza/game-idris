module WithFields

public export
interface WithFields a where
  type : a -> String
  getKV : a -> List (String, String)

public export
%overlapping
WithFields a => Show a where
  show some = type some
  -- show some = "{ " ++ type ++ " | " ++ "}"
