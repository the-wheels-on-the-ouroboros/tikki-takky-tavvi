module Utilities (maximumBy) where


maximumBy : (a -> comparable') -> List a -> Maybe a
maximumBy translate list =
    let
        maxComparison =
            \element currentMaxElement ->
                if (translate element) < (translate currentMaxElement)
                    then currentMaxElement
                    else element
    in
        case list of
            hd :: tl -> Just (List.foldl maxComparison hd tl)
            [] -> Nothing
