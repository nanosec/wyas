module Wyas.Primitives (allPrimitives) where

import Wyas.Primitives.IO (ioPrimitives)
import Wyas.Primitives.List (listPrims)
import Wyas.Primitives.Number (numberPrims)
import Wyas.Primitives.Predicate (predicates)
import Wyas.Primitives.String (stringPrims)
import Wyas.Types

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = listPrims ++ numberPrims ++ predicates ++ stringPrims

allPrimitives :: [(String, LispVal)]
allPrimitives = map (makeValue PrimitiveFunc) primitives ++
                map (makeValue IOFunc       ) ioPrimitives
    where makeValue constructor (var, func) = (var, constructor func)
