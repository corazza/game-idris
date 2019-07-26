module Server.Rules.NumericProperties

import Descriptions.ObjectDescription.RulesDescription
import Data.AVL.DDict
import GameIO
import Objects

public export
record NumericProperty where
  constructor MkNumericProperty
  current : Double
  full : Double
%name NumericProperty numericProperty

export
Show NumericProperty where
  show np = show (current np) ++ "/" ++ show (full np)

numericPropFromDescription : NumericPropertyDescription ->
                             (current : Double) ->
                             NumericProperty
numericPropFromDescription desc current
  = MkNumericProperty current (full desc)

export
waste : (amount : Double) -> NumericProperty -> NumericProperty
waste amount = record { current $= flip (-) amount }

export
fill : (amount : Double) -> NumericProperty -> NumericProperty
fill amount = record { current $= (+) amount }

export
wasteFull : NumericProperty -> NumericProperty
wasteFull = record { current = 0 }

export
fillUp : NumericProperty -> NumericProperty
fillUp np = record { current = full np } np

public export
NumericPropertyDict : Type
NumericPropertyDict = DDict NumericPropertyId NumericProperty
%name NumericPropertyDict numericProperties

numPropIdDesc : (NumericPropertyId, NumericPropertyDescription) ->
                (NumericPropertyId, NumericProperty)
numPropIdDesc (id, desc) = (id, numericPropFromDescription desc (full desc))

export
numPropDictFromDescription : RulesDescription -> Maybe NumericPropertyDict
numPropDictFromDescription desc
  = map (fromList . map numPropIdDesc . toList) $ numericProperties desc

export
addNumericProperty : NumericPropertyId ->
                     NumericProperty ->
                     NumericPropertyDict ->
                     NumericPropertyDict
addNumericProperty = insert

export
removeNumericProperty : NumericPropertyId -> NumericPropertyDict -> NumericPropertyDict
removeNumericProperty = delete

export
updateNumericProperty : NumericPropertyId ->
                        (f : NumericProperty -> NumericProperty) ->
                        NumericPropertyDict -> NumericPropertyDict
updateNumericProperty id f = update id f

export
queryNumericProperty : NumericPropertyId ->
                       (q : NumericProperty -> a) ->
                       NumericPropertyDict -> Maybe a
queryNumericProperty id q dict = lookup id dict >>= pure . q
