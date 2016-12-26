module CommonUtil where

while  :: (a->(Bool,a)) -> a -> a
while fun state = let (isContinue,newState) = fun state in if isContinue then while fun newState else newState
