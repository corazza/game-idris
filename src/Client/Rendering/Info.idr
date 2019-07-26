module Client.Rendering.Info

import Data.AVL.Dict

import Objects
import Descriptions
import Descriptions.ObjectDescription.RulesDescription

public export
record NumericPropertyInfo where
  constructor MkNumericPropertyInfo
  current : Double
  full : Double

public export
NumPropInfoDict : Type
NumPropInfoDict = Dict NumericPropertyId NumericPropertyInfo

descToInfo : NumericPropertyDescription -> NumericPropertyInfo
descToInfo desc = MkNumericPropertyInfo (full desc) (full desc)

export
updateNumericPropertyInDict : NumericPropertyId ->
                              Double ->
                              NumPropInfoDict -> NumPropInfoDict
updateNumericPropertyInDict prop_id new = update prop_id (record { current = new })

public export
record ObjectInfo where
  constructor MkObjectInfo
  numericProperties : Maybe NumPropInfoDict

export
updateNumericProperty : NumericPropertyId ->
                        Double ->
                        ObjectInfo -> ObjectInfo
updateNumericProperty prop_id current
  = record { numericProperties $= map (updateNumericPropertyInDict prop_id current) }

export
objectInfoFromRulesDescription : RulesDescription -> Maybe ObjectInfo
objectInfoFromRulesDescription
  = map (MkObjectInfo . Just) . map (map descToInfo) . numericProperties
