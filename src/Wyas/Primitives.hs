module Wyas.Primitives (primitives) where

import Wyas.Primitives.IO (ioPrimitives)
import Wyas.Primitives.List (listPrims)
import Wyas.Primitives.Number (numberPrims)
import Wyas.Primitives.Predicate (predicates)
import Wyas.Primitives.String (stringPrims)
import Wyas.Types

purePrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
purePrimitives = listPrims ++ numberPrims ++ predicates ++ stringPrims

primitives :: [(String, LispVal)]
primitives = map (makeValue PrimitiveFunc) purePrimitives ++
             map (makeValue IOFunc       ) ioPrimitives
    where makeValue constructor (var, func) = (var, constructor func)
