module Utility where

-- composition operator for 2 args functions
(...) ::  (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(...) = (.) . (.)