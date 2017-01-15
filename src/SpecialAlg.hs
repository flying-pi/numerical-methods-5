module SpecialAlg where

import CommonUtil

data ChebushovData = ChebushovData {
    xn :: [Double],
    result :: Double,
    integtal :: Double -> Double,
    value :: Double
  }

newton :: Double -> Double -> Double -> (Double -> Double) -> Double
newton a b count fun = ((b - a)/2) * integralValue
  where
    operand1 = (b+a)/2
    operand2 = (b-a)/2
    h = ((b-a)/count)
    integralValue = result ( while (\itState -> (xn itState == [], itState {
        xn = tail$xn$itState,
        result = result itState + (head$xn$itState)*(integtal itState (value itState)),
        value = value itState + h
      })) ChebushovData {
        xn = if count == 4 then
            [7.0/45.0,32.0/45.0,12.0/45.0,32.0/45.0,7.0/45.0]
          else
            [19.0/144.0,75.0/144.0,50.0/144.0,50.0/144.0,75.0/144.0,19.0/144.0],
        result =0,
        value = a,
        integtal = fun})


hause :: Double -> Double -> Double -> (Double -> Double) -> Double
hause a b count fun = ((b - a)/2) * integralValue
  where
    operand1 = (b+a)/2
    operand2 = (b-a)/2
    h = ((b-a)/count)*2
    integralValue = result ( while (\itState -> (xn itState == [], itState {
        xn = tail$xn$itState,
        result = result itState + (head$xn$itState)*(integtal itState (value itState))
      })) ChebushovData {
        xn =
          (case count of
            4 ->  [7.0/45.0,32.0/45.0,12.0/45.0,32.0/45.0,7.0/45.0]
            5 ->  [7.0/45.0,32.0/45.0,12.0/45.0,32.0/45.0,7.0/45.0]
            _ ->  [7.0/45.0,32.0/45.0,12.0/45.0,32.0/45.0,7.0/45.0]
        ),
        result =0,
        value = operand1,
        integtal = fun})


chebushov :: Double -> Double -> Double -> (Double -> Double) -> Double
chebushov a b count fun = ((b - a)/count) * integralValue
  where
    operand1 = (b+a)/2
    operand2 = (b-a)/2
    nodes =(case count of
                4 ->  [-0.794655,-0.187593,0.187593,0.794655]
                5 ->  [-0.832498,-0.374541,0.0,0.374541,0.832498]
                _ ->  [-0.866257,-0.422519,-0.266636,0.266636,0.422519,0.866257]
            )
    integralValue = result ( while (\itState -> (xn itState == [], itState {
        result = (result itState) + (integtal itState (operand1 + operand2 *(head (xn itState)))),
        xn = tail$xn$itState
      })) ChebushovData {
        xn = nodes,
        result =0,
        value = operand1 ,
        integtal = fun})



