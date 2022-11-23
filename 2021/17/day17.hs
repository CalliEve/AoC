import Data.Maybe
import Debug.Trace

iter :: (Int, Int, Int, Int) -> [Int] -> Maybe Int
iter (x, y, xv, yv) ys | Just b <- inTarget (x, y, xv) = if b then Just $ maximum ys else iter (x + xv, y + yv, if xv < 0 then xv + 1 else if xv == 0 then 0 else xv - 1,yv-1) (y:ys)
                       | otherwise = Nothing

inTarget :: (Int, Int, Int) -> Maybe Bool
inTarget (x, y, xv) = if (x > 273 || xv == 0) && y < (-97) then Nothing else Just (x >= 241 && x <= 273 && y <= (-63) && y >= (-97))

calc :: (Int, Int) -> Maybe Int
calc (xv, yv) = iter (0, 0, xv, yv) []

solve :: Int
solve =  length $ catMaybes $ map calc $ [(x, y) | x <- [0..273], y <- [(-97)..97]]

main = print $ solve
