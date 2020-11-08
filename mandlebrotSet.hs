-- bounded  ░
-- <= 8 ▒
-- <= 20 ▓
--  else █
--  

boundedSpaceSymbol = "░"

levelOneExplosionSymbol = "▒" 

levelTwoExplosionSymbol = "▓"

levelThreeExplosionSymbol = "█"
raisedPowerTwoBoundedValue = 4


--- values to exploit
levelOneExplosionValue = 200

levelTwoExplosionValue = 20

levelThreeExplosionValue = 8


-- [-2; 2] - interval
leftSideMax = -2
rightSideMax = 2
upSideMax = 1
downSideMax = -1
estimationMark = 0.019
maximumNumberOfIterations = 50

epsilon = 0.0001

---- all the values to exploit are up here ^
calculateGrowth z c numberOfIterations = 
    let currentResult = calculateMandlebrot z c
    in let currentResultLength = estimateVectorRaisedLength currentResult
        in if currentResultLength < epsilon || numberOfIterations == maximumNumberOfIterations
            then (currentResultLength, numberOfIterations)
            else if (checkBoundary currentResultLength) 
                then (currentResultLength, numberOfIterations)
                else calculateGrowth currentResult c (numberOfIterations + 1)

estimateVectorRaisedLength (x, y) = x * x + y * y

checkBoundary vectorLengthRaised = vectorLengthRaised > raisedPowerTwoBoundedValue

calculateMandlebrot (zA, zB) (cX, cY) = 
    (zA * zA - (zB *zB) + cX , cY + 2 * zA * zB) 


assignColourForSpace (calculationValue, numberOfIterations) 
    | calculationValue <= raisedPowerTwoBoundedValue =  boundedSpaceSymbol
    | numberOfIterations <= levelThreeExplosionValue = levelThreeExplosionSymbol
    | numberOfIterations <= levelTwoExplosionValue = levelTwoExplosionSymbol
    | otherwise = levelOneExplosionSymbol

iterateSpace x y string
      | x > rightSideMax = iterateSpace leftSideMax (y - estimationMark) (string ++ "\n")
      | y < downSideMax = string
      | otherwise = 
          let currentGrowth = calculateGrowth ((0, 0)) ((x, y)) 1
          in iterateSpace (x + estimationMark) y (string ++ ((assignColourForSpace currentGrowth)))
          
main = putStrLn (iterateSpace leftSideMax upSideMax "")