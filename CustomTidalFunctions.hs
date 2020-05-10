import Sound.Tidal.Context
import Sound.Tidal.Chords
import Sound.Tidal.Utils

setbpm a = setcps (a/60/4)
-- e = euclidFull

bpm a = cps (a/60/4)

-- snowball :: (Pattern a -> Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Int -> Pattern a -> Pattern a
-- snowball depth combinationFunction f pattern = cat $ take depth $ scanl combinationFunction pattern $ iterate f pattern

fastsnowball depth combinationFunction f pattern = fastcat $ take depth $ scanl combinationFunction pattern $ iterate f pattern

cycleChooseBy = segment 1 . cycleChooseBy

-- soak :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
-- soak depth f pattern = cat $ take depth $ iterate f pattern

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


chordList :: String
chordList = unwords $ map fst (chordTable :: [(String, [Int])])



slowrun :: (Real a, Enum a)=>Pattern a -> Pattern a
slowrun n = slow (fmap toTime n) $ run ( n)



chooseBy :: Pattern Double -> [a] -> Pattern a
chooseBy _ [] = silence
chooseBy f xs = (xs !!!) . floor <$> range 0 (fromIntegral $ length xs) f


ghostBy a p = tParam ghost' (a) p

ghostByWith = ghost''

ghostWith fx = ghostByWith 0.125 (fx.((|*| gain (pure 0.7)) . (# end (pure 0.2)) . (|*| speed (pure 1.25))))

replicator text1 = [putStr (text1) | x <- replicate 500 text1]

flood text2 = sequence_(replicator text2)

replicator' n text1 = [putStr (text1) | x <- replicate n text1]

flood' n text2 = sequence_(replicator' n text2)


terr :: Time -> Time -> Pattern a -> (Time,Time,Pattern a)
terr start stop pattern = (start, stop, pattern)


-- thanks eric
-- whenmodr :: [Pattern Time] -> [Int] -> [Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a

mode n scaleName p = (|- (scale scaleName (n-1))) $ scale scaleName $ (+ n) $ p

invert p = (|* "-1") $ p

mix fx p = ( p + fx p)

condFx bools fx p = sew bools (fx p) p


foldmod ::  [Pattern Time] -> Int -> Int -> (Pattern b -> Pattern b) -> Pattern b -> Pattern b
foldmod [] _ _ _ p = p
foldmod timescales d n fx p = foldmod (tail timescales) d n fx $ outside (head timescales) (whenmod d n fx) $ p

foldwhen :: [Int] -> [Int] -> (Pattern b -> Pattern b) -> Pattern b -> Pattern b
foldwhen [] _ _ p = p
foldwhen _ [] _ p = p
foldwhen ds ns fx p = if (done) then (pfx) else (foldwhen restds restns fx $ pfx)
                    where restns = if null (tail ns) then [n] else (tail ns)
                          restds = if null (tail ds) then [d] else (tail ds)
                          n = head ns
                          d = head ds
                          pfx = whenmod d n fx $ p
                          done = (null $ tail ns) && (null $ tail ds)

whenmodr :: [Pattern Time] -> [Int] -> [Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
whenmodr speeds numerators denominators modifier pattern = if (done) then (modifiedpattern) else (whenmodr rests restn restd modifier modifiedpattern)
                                                            where modifiedpattern = outside speed (whenmod numerator denominator (modifier)) $ pattern
                                                                  numerator = (head numerators)
                                                                  denominator = (head denominators)
                                                                  speed = (head speeds)
                                                                  done = (null $ tail speeds) && (null $ tail numerators) && (null $ tail denominators)
                                                                  restn = if null (tail numerators) then [numerator] else (tail numerators)
                                                                  restd = if null (tail denominators) then [denominator] else (tail denominators)
                                                                  rests = if null (tail speeds) then [speed] else (tail speeds)

isoe :: Pattern Int -> Pattern Int -> Pattern Time -> Pattern a -> Pattern a
isoe n d s p = slow ((fmap toTime d)/s) $ euclid n d $ p



parade :: Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
parade depth offset fx p = splitQueries $ p {query = q}
          where q st = query (head $ rotateList (currentCycleCount + offset) $ take depth $ iterate fx p) st
                          where currentCycleCount = (floor $ start $ arc st)
                rotateList _ [] = []
                rotateList n xs = zipWith const (drop n (cycle xs)) xs



m = (const silence)


mkpat name pattern = (name,pattern)

mkfx name fx = (name,fx)

sendMidiClock = p "clock" $ fast 2 $ midicmd "midiClock*48" #s "midi";

sendMidiStop = once $ midicmd "stop" #s "midi"

sendMidiStart = once $ midicmd "stop" #s "midi"
