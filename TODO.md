# Typed references

A large number of game rules concern ownership and transferring of references, which are currently strings and thus the absence of error states cannot be guaranteed.

- level 1: plain sum type as ref type
- level 2: more data as ref type
- level 3 (?): function computing the ref type
- level 4: use dependent types somehow

# Rest

1. Click strings and smarter matching
  -
2. Simplify communication between components. Too many command types, i.e. they're too confusing.
3. Marshalling: use a plain monad instead of Control.ST
