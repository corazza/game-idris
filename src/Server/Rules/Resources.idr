module Server.Rules.Resources

public export
record Resource where
  constructor MkResource
  current : Double
  full : Double
%name Resource resource

public export
Resources : Type
Resources = DDict ContentReference Resource
%name Resources resources

export
addResource : String
