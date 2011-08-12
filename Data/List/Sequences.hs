--
-- Copyright (C) 2011 by Jonas Kramer. All rights reserved.
-- Published under the terms of the GNU General Public License (GPL).
--

-- | Find sequences within lists.

module Data.List.Sequences where

    -- | Find sequences within a list and return them as new list of sequences.
    -- The first argument is a function that takes two subsequent elements of the
    -- given list (second argument) and returns whether the second element
    -- follows the first one in a sequence.
    --
    -- > splitSeq ((==) . succ) [1,2,3,5,6,7]
    -- > [[1,2,3],[5,6,7]]
    --
    splitSeq :: (a -> a -> Bool) -> [a] -> [[a]]
    splitSeq _ [] = []
    splitSeq f xs = group : splitSeq f rest where (group, rest) = spanSeq f xs


    -- | Works pretty much like `splitSeq`, except that a tuple with only the
    -- sequence starting at the first element and the rest of the list is
    -- returned.
    --
    -- > spanSeq ((==) . succ) "abcxyz123"
    -- > ("abc","xyz123")
    --
    spanSeq :: (a -> a -> Bool) -> [a] -> ([a], [a])
    spanSeq _ [] = ([], [])
    spanSeq _ (x:[]) = ([x], [])
    spanSeq f (x:xs) =
        split' x xs
        where
            split' x [] = ([x], [])
            split' x (y:ys)
                | f x y = (x : s, rest)
                | otherwise = ([x], y : ys)
                where
                    (s, rest) = split' y ys
