
module Methods where

import CommonUtil

data RectangleIteration = RectangleIteration {
    value :: Double,
    result :: Double,
    maxV :: Double,
    step :: Double,
    integtal :: Double -> Double
  }

rectangleLeft::Double -> Double -> Double -> (Double -> Double) -> Double
rectangleLeft a b count fun =
  result (while (\ itState -> ( (value itState)  + (step itState ) < (maxV itState ),
    itState {
      value = (value itState) + (step itState ),
      result = ((result itState) + ((integtal itState (value itState)) * (step itState)))}))
    RectangleIteration {value = a, result = 0, Methods.maxV = b, step = (b - a) / count, integtal = fun})


rectangleRight::Double -> Double -> Double -> (Double -> Double) -> Double
rectangleRight a b count fun =
  result (while (\ itState -> let newValue = (value itState) + (step itState ) in  ( (value itState)  + (step itState ) < (maxV itState ),
    itState {
      value = newValue,
      result = ((result itState) + ((integtal itState newValue) * (step itState)))})  )
    RectangleIteration {value = a, result = 0, Methods.maxV = b, step = (b - a) / count, integtal = fun})


rectangleCenter::Double -> Double -> Double -> (Double -> Double) -> Double
rectangleCenter a b count fun =
  result (while (\ itState -> ( (value itState)  + (step itState ) < (maxV itState ),
    itState {
      value = (value itState) + (step itState ),
      result = ((result itState) + ((integtal itState ((value itState)+(step itState)/2)) * (step itState)))}))
    RectangleIteration {value = a, result = 0, Methods.maxV = b, step = (b - a) / count, integtal = fun})

trapeze::Double -> Double -> Double -> (Double -> Double) -> Double
trapeze a b count fun =
  h*(((fun a) + (fun b))/2 + expression)
  where
      h = (b-a)/count
      expression = result (while (\ itState -> ( (value itState)  + (step itState ) < (maxV itState ),
                  itState {
                    value = (value itState) + (step itState ),
                    result = ((result itState) + ((integtal itState (value itState))))}))
                  RectangleIteration {value = a, result = 0, Methods.maxV = b-h, step = (b - a) / count, integtal = fun})

parabola::Double -> Double -> Double -> (Double -> Double) -> Double
parabola a b count fun =
  h*((fun a) + (fun b) + 4*sum1 + 2*sum2)/3
  where
      h = ((b - a) / 2 )/count
      sum1 = result (while (\ itState -> ( (value itState)  + (step itState ) < (maxV itState ),
                  itState {
                    value = (value itState) + (step itState )*2,
                    result = ((result itState) + ((integtal itState ( (value itState) + (step itState )*2))))}))
                  RectangleIteration {value = a-h, result = 0, Methods.maxV = b, step = h, integtal = fun})
      sum2 = result (while (\ itState -> ( (value itState)  + (step itState ) < (maxV itState ),
                  itState {
                    value = (value itState) + (step itState )*2,
                    result = ((result itState) + ((integtal itState ((value itState) + (step itState )*2))))}))
                  RectangleIteration {value = a, result = 0, Methods.maxV = b-h, step = h, integtal = fun})
