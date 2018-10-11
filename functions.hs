import Sound.Tidal.Context
import qualified Sound.Tidal.Scales as Scales
import qualified Sound.Tidal.Chords as Chords

-- myfunc  =  sound $ Pattern "bd"
myfunc  = sometimes (foldEvery [2,4] degrade)
  -- $ sometimes (foldEvery [9,10,11]  (const $ (0.25 ~>) $ sound "c64hh*2" # gain (scalex 0.5 1 $ (run 16)/16)))
  -- $ foldEvery [3,5] (every 4 ( ("[0.125 0.25 0.5]/2" ~>) . often (stut 3 0.88 6 .slow 4.palindrome.(|*|speed 0.5))))
  -- $ rarely (every 4 (const $ sound "sn:2*4" # gain "[0.25 0.6  0.5 1]"))
  -- $ sometimes (foldEvery [3, 4] ( ("0.125 0 0.125" <~)))
  -- $ stack [
  --   juxBy 0.5 ((|+| up (rand)).chop 4) $ sound ". drum:1",
  --   sometimesBy 0.5 ( foldEvery [3,7,8] ((rarely ( palindrome.  stut 3 0.5 16) ) . (slow $ slow 4 "2 [3 2]")))
  --     $ rarely (every 4 (striate 4))
  --     $ sometimes (every 4 (const
  --       $ randcat [
  --         sound "[c64sn:3 sn:2] [ . c64hh*3] c64bd:7 c64cp",
  --         sound "c64sn*6" # gain ((run 8 )/17 + 0.6),
  --         sound "[[. c64hh] c64bd:7 [c64bd[c64bd[c64bd]]]] {{[c64cb*3]/2}%2,[. . . c64hh:0*4]} ",
  --         sound "[[. c64hh] c64bd:7 [c64bd[c64bd[c64bd]]]] [. c64sn],[. . . c64hh:0*4]} "
  --       ]))
  --   $ slow 3
    -- $ stack [
    --   sound $ Pattern "{<{c64bd}%3, [ . c64hh]> [c64sn [c64hh c64cp*3]]}%3",
    --   stut 2 0.5 3 $ (0.125 ~>) $  sound $ Pattern "{[c64hh:4*2]*[[3 4] [2 1]]}%3" |+| n ( scale 1 0 $ irand 6) # gain 0.9
    -- ]
  -- ]

testFun = stack [ sound $ Pattern "{<{c64bd}%3, [ . c64hh]> [c64sn [c64hh c64cp*3]]}%3"]

main = putStrLn "Hello, World!"
