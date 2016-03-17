module Utilities where


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


areElementsEqual : List a -> List a -> Bool
areElementsEqual list list' =
    (List.length list) == (List.length list')
        && List.all (\element -> (count element list) == (count element list')) list


count : a -> List a -> Int
count element list =
    List.foldl (\element' total -> if element == element' then total + 1 else total) 0 list
