module Wyas.Primitives
    (
      primitives
    , ioPrimitives
    ) where

import Wyas.Primitives.IO (ioPrimitives)
import Wyas.Primitives.List (listPrims)
import Wyas.Primitives.Number (numberPrims)
import Wyas.Primitives.Predicate (predicates)
import Wyas.Primitives.String (stringPrims)
import Wyas.Types

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = listPrims ++ numberPrims ++ predicates ++ stringPrims
