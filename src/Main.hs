module Main where

import Control.Lens

data Vector = Vector
    { x :: Float
    , y :: Float
    } deriving Show
    
_x :: Lens' Vector Float
_x = lens 
    x 
    (\(Vector a b) c -> Vector c b)

_theta :: Lens' Vector Float
_theta = lens
    (\(Vector a b) -> atan (b/a))
    (\(Vector a b) t2 ->
        let mag = sqrt ((a^2) + (b^2))
        in  Vector (mag*(cos t2)) (mag*(sin t2))
    )

_mag :: Lens' Vector Float
_mag = lens 
    (\(Vector a b) -> sqrt (a^2+b^2))
    (\vect new_mag -> 
        let theta  = view _theta vect
            unit_x = cos theta
            unit_y = sin theta
        in  Vector (new_mag*unit_x) (new_mag*unit_y)
    )

main :: IO ()
main = do
  putStrLn "hello world"
