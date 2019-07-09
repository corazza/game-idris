||| Wrapper on Data.AVL.Dict for phony deletion
module Data.AVL.DDict

import Data.AVL.Dict

%default total
%access public export

data DDict : (k : Type) -> Type -> Type where
  MkDDict : Dict k (Maybe v) -> DDict k v

empty : Ord k => DDict k v
empty = MkDDict empty

insert : (Ord k) => k -> v -> DDict k v -> DDict k v
insert key val (MkDDict dict) = MkDDict $ insert key (Just val) dict

update : (Ord k) => k -> (v -> v) -> DDict k v -> DDict k v
update key f (MkDDict dict) = MkDDict $ update key (map f) dict

delete : (Ord k) => k -> DDict k v -> DDict k v
delete key (MkDDict dict) = MkDDict $ update key (const Nothing) dict

toList : DDict k v -> List (k,v)
toList (MkDDict dict) = catMaybes $ moveJust $ Dict.toList dict where
  moveJust : List (k, Maybe v) -> List (Maybe (k, v))
  moveJust [] = []
  moveJust ((key, Nothing) :: xs) = Nothing :: moveJust xs
  moveJust ((key, Just val) :: xs) = Just (key, val) :: moveJust xs

fromList : Ord k => List (k,v) -> DDict k v
fromList kvs = let kvs' = map (\(a, b)=>(a, Just b)) kvs in
                        MkDDict (Dict.fromList kvs')

isEmpty : DDict k v -> Bool
isEmpty (MkDDict dict) = isEmpty dict

foldr : (k -> v -> p -> p) -> p -> DDict k v -> p
foldr f init (MkDDict dict) = Dict.foldr withJust init dict where
  withJust : (k -> Maybe v -> p -> p)
  withJust key Nothing p = p
  withJust key (Just val) p = f key val p

lookup : (Ord k) => k -> DDict k v -> Maybe v
lookup key (MkDDict dict) = case lookup key dict of
  Nothing => Nothing
  (Just x) => x

isIn : Ord k => Dict k (Maybe v) -> k -> Bool
isIn dict key = case lookup key dict of
  Nothing => False
  Just x => isJust x

keys : Ord k => DDict k v -> List k
keys (MkDDict dict) = filter (isIn dict) (keys dict)

values : DDict k v -> List v
values (MkDDict dict) = catMaybes $ values dict

size : DDict k v -> Nat
size = foldr (\_,_=>S) 0

hasKey : (Ord k) => k -> DDict k v -> Bool
hasKey key (MkDDict dict) = if hasKey key dict
  then case lookup key dict of -- check if key was deleted
    Just (Just x) => True
    _ => False
  else False

hasValue : (Eq v) => v -> DDict k v -> Bool
hasValue val (MkDDict dict) = hasValue (Just val) dict


-- (Eq k, Eq v) => Eq (DDict k v) where
--    (==) (MkDict {h = h} x) (MkDict {h = h'} y) with (decEq h h')
--      (==) (MkDict {h = h} x) (MkDict {h = h} y)  | Yes Refl = x == y
--      (==) (MkDict {h = h} x) (MkDict {h = h'} y) | No _     = False

-- (Show k, Show v) => Show (Dict k v) where
--   show (MkDDict dict) = show dict

Ord a => Functor (DDict a) where
  map func x = fromList $ (\(a, b) => (a, func b)) <$> toList x

Ord a => Foldable (DDict a) where
  foldr func = foldr $ const func

Ord a => Traversable (DDict a) where
  traverse f x = fromList <$> (traverse (\(a, b) => (MkPair a) <$> f b) $ DDict.toList x)
