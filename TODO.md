# Hardening

1. Try compiling on other computers, then on other Idris versions
2. Stress-tests

# Goals

1. Finish object editor

# Improvements

## Typed references

A large number of game rules concern ownership and transferring of references, which are currently strings and thus the absence of error states cannot be guaranteed.

- level 1: plain sum type as ref type
- level 2: more data as ref type
- level 3 (?): function computing the ref type
- level 4: use dependent types somehow

## Click strings and smarter matching

## Commands

Simplify communication between components. Too many command types, i.e. they're too confusing.

## Marshalling

Use a plain monad instead of Control.ST
