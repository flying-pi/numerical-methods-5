module SpecialAlg where

import CommonUtil

data ChebushovData = ChebushovData {
    xn :: [Double]
    result :: Double,
    n :: Integer
    integtal :: Double -> Double
  }

-- chebushov::Double -> Double -> Double -> (Double -> Double) -> Double
-- chebushov a b count fun =
--   ((b-a)/count)*sum1
--   where
--     sum1 = result (while (\ itState -> ((n itState )<=0 ,
--         itState {
--
--           })))




--
--       sum1 = result (while (\ itState -> ( (value itState)  + (step itState ) < (maxV itState ),
--                   itState {
--                     value = (value itState) + (step itState )*2,
--                     result = ((result itState) + ((integtal itState ( (value itState) + (step itState )*2))))}))
--                   RectangleIteration {value = a-h, result = 0, Methods.maxV = b, step = h, integtal = fun})