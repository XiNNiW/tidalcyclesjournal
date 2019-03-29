import Sound.Tidal.Context
import Sound.Tidal.Chords

-- snowball :: (Pattern a -> Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Int -> Pattern a -> Pattern a
snowball depth combinationFunction f pattern = cat $ take depth $ scanl combinationFunction pattern $ iterate f pattern

fastsnowball depth combinationFunction f pattern = fastcat $ take depth $ scanl combinationFunction pattern $ iterate f pattern

-- soak :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
soak depth f pattern = cat $ take depth $ iterate f pattern

fastsoak depth f pattern = fastcat $ take depth $ iterate f pattern

-- cascade :: [Pattern a] -> Pattern a
-- cascade voices = (stack $ delayEntry voices
--                     where delay = toTime (1%n)
--                           n =  length voices
--                           transform = rotR delay
--                           delayEntry (v:vs) = v : (delayEntry $ map transform vs)
--                           delayEntry [] = [])

rip a b p = within ( 0.25, 0.75) (slow 2 . rev . stut 8 a b) p

laceWith fx p = interlace p (fx p)

one p = stut' 2 (0.125/2) (|* gain "1") $ p

backrush speed = within (0.75, 1)(rev.stut 4 0.66 (1/speed))

-- chordList :: String
chordList = unwords $ map fst (chordTable :: [(String, [Int])])

ghostBy a p = tParam ghost' (a) p

replicator text1 = [putStr (text1) | x <- replicate 500 text1]

flood text2 = sequence_(replicator text2)

replicator' n text1 = [putStr (text1) | x <- replicate n text1]

flood' n text2 = sequence_(replicator' n text2)

-- terr :: Time -> Time -> Pattern a -> (Time,Time,Pattern a)
terr start stop pattern = (start, stop, pattern)

-- thanks eric
whenmodr :: [Pattern Time] -> [Int] -> [Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
whenmodr speeds numerators denominators modifier pattern
    | done = modifiedpattern
    | otherwise =  whenmodr rests restn restd modifier modifiedpattern
    where modifiedpattern = outside speed (whenmod numerator denominator (modifier)) $ pattern
          numerator = (head numerators)
          denominator = (head denominators)
          speed = (head speeds)
          done = (null $ tail speeds) && (null $ tail numerators) && (null $ tail denominators)
          restn = if null (tail numerators) then [numerator] else (tail numerators)
          restd = if null (tail denominators) then [denominator] else (tail denominators)
          rests = if null (tail speeds) then [speed] else (tail speeds)

whenmods' speeds numerators denominators modifier pattern
    | done = modifiedpattern
    | otherwise =  whenmods' rests restn restd modifier modifiedpattern
    where modifiedpattern = outside speed (whenmod numerator denominator ((fast speed).(modifier))) $ pattern
          numerator = (head numerators)
          denominator = (head denominators)
          speed = (head speeds)
          done = (null $ tail speeds) && (null $ tail numerators) && (null $ tail denominators)
          restn = if null (tail numerators) then [numerator] else (tail numerators)
          restd = if null (tail denominators) then [denominator] else (tail denominators)
          rests = if null (tail speeds) then [speed] else (tail speeds)

whenmods speeds numerators denominators newpattern pattern = whenmods' speeds numerators denominators (const newpattern) pattern

mkpat name pattern = (name,pattern)

mkfx name fx = (name,fx)
