module Data.AVL.SetRemove

import Data.AVL.Set

export
remove : Ord a => a -> Set a -> Set a
remove x = let to_remove = insert x empty
               in flip difference to_remove
