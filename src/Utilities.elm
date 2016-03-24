module Utilities (maximumBy) where


maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy evaluate list =
    let
        maxComparison =
            \element currentMax ->
                if (evaluate element) < (evaluate currentMax)
                    then currentMax
                    else element
    in
        case list of
            head :: tail -> Just (List.foldl maxComparison head tail)
            [] -> Nothing
