:set -XOverloadedStrings
:set -XFlexibleContexts
:set prompt ""


import Sound.Tidal.Context
import Sound.Tidal.Chords

-- :module Sound.Tidal.Context
-- -- -- :set prompt-cont ""
-- --
-- -- import Sound.Tidal.Context
-- -- import Sound.Tidal.Chords
--
-- :load "~/Music/tidal-cycles/lib"



-- total latency = oLatency + cFrameTimespan
tidal <- startTidal (superdirtTarget {oLatency = 0.3, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20})
-- tidal <- startTidal (superdirtTarget {oLatency = 0.02}) (defaultConfig {cFrameTimespan = 1/20, cTempoAddr = "192.168.0.105"})

let p = streamReplace tidal
let hush = streamHush tidal
let list = streamList tidal
let mute = streamMute tidal
let unmute = streamUnmute tidal
let solo = streamSolo tidal
let unsolo = streamUnsolo tidal
let once = streamOnce tidal False
let asap = streamOnce tidal True
let nudgeAll = streamNudgeAll tidal
let all = streamAll tidal
let resetCycles = streamResetCycles tidal
let setcps = asap . cps
let xfade i = transition tidal (Sound.Tidal.Transition.xfadeIn 4) i
let xfadeIn i t = transition tidal (Sound.Tidal.Transition.xfadeIn t) i
let histpan i t = transition tidal (Sound.Tidal.Transition.histpan t) i
let wait i t = transition tidal (Sound.Tidal.Transition.wait t) i
let waitT i f t = transition tidal (Sound.Tidal.Transition.waitT f t) i
let jump i = transition tidal (Sound.Tidal.Transition.jump) i
let jumpIn i t = transition tidal (Sound.Tidal.Transition.jumpIn t) i
let jumpIn' i t = transition tidal (Sound.Tidal.Transition.jumpIn' t) i
let jumpMod i t = transition tidal (Sound.Tidal.Transition.jumpMod t) i
let mortal i lifespan release = transition tidal (Sound.Tidal.Transition.mortal lifespan release) i
let interpolate i = transition tidal (Sound.Tidal.Transition.interpolate) i
let interpolateIn i t = transition tidal (Sound.Tidal.Transition.interpolateIn t) i
let clutch i = transition tidal (Sound.Tidal.Transition.clutch) i
let clutchIn i t = transition tidal (Sound.Tidal.Transition.clutchIn t) i
let anticipate i = transition tidal (Sound.Tidal.Transition.anticipate) i
let anticipateIn i t = transition tidal (Sound.Tidal.Transition.anticipateIn t) i
let d1 = p 1
let d2 = p 2
let d3 = p 3
let d4 = p 4
let d5 = p 5
let d6 = p 6
let d7 = p 7
let d8 = p 8
let d9 = p 9
let d10 = p 10
let d11 = p 11
let d12 = p 12
let d13 = p 13
let d14 = p 14
let d15 = p 15
let d16 = p 16


-- snowball :: (Pattern a -> Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Int -> Pattern a -> Pattern a
-- snowball depth combinationFunction f pattern = cat $ take depth $ scanl combinationFunction pattern $ iterate f pattern

fastsnowball depth combinationFunction f pattern = fastcat $ take depth $ scanl combinationFunction pattern $ iterate f pattern


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

:{
chordList :: String
chordList = unwords $ map fst (chordTable :: [(String, [Int])])
:}

:{
slowrun :: (Real a, Enum a)=>Pattern a -> Pattern a
slowrun n = slow (fmap toTime n) $ run ( n)
:}

ghostBy a p = tParam ghost' (a) p

replicator text1 = [putStr (text1) | x <- replicate 500 text1]

flood text2 = sequence_(replicator text2)

replicator' n text1 = [putStr (text1) | x <- replicate n text1]

flood' n text2 = sequence_(replicator' n text2)

:{
terr :: Time -> Time -> Pattern a -> (Time,Time,Pattern a)
terr start stop pattern = (start, stop, pattern)
:}

-- thanks eric
-- whenmodr :: [Pattern Time] -> [Int] -> [Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a

mode n scaleName p = (|- (scale scaleName (n-1))) $ scale scaleName $ (+ n) $ p

invert p = (|* "-1") $ p

mix fx p = ( p + fx p)

condFx bools fx p = sew bools (fx p) p

:{
  foldmod ::  [Pattern Time] -> Int -> Int -> (Pattern b -> Pattern b) -> Pattern b -> Pattern b
  foldmod [] _ _ _ p = p
  foldmod timescales d n fx p = foldmod (tail timescales) d n fx $ outside (head timescales) (whenmod d n fx) $ p
:}

:{
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
:}

:{
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
:}

:{
isoe :: Pattern Int -> Pattern Int -> Pattern Time -> Pattern a -> Pattern a
isoe n d s p = slow ((fmap toTime d)/s) $ euclid n d $ p
:}

m = (const silence)

mkpat name pattern = (name,pattern)

mkfx name fx = (name,fx)

sendMidiClock = p "clock" $ fast 2 $ midicmd "midiClock*48" #s "midi";

sendMidiStop = once $ midicmd "stop" #s "midi"

sendMidiStart = once $ midicmd "stop" #s "midi"



:set prompt "tidal> "
