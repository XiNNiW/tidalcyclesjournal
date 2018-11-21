
module MyFunctions (unpackChordString,createChordProgression) where
  import Sound.Tidal.Context
  -- import qualified Sound.Tidal.Chords as Chords
  import Data.Maybe

  unpackChordString :: String -> [(String,String)]
  unpackChordString "" = []
  unpackChordString " " = []
  unpackChordString chordString = map (splitOn ':') (words chordString)
    where splitOn sep str = splitAt (fromJust $ elemIndex sep str)
                            $ filter (/= sep) str
                            -- .(filter (/= '[')).(filter (/= ']'))) str


  createChordProgression :: String -> Pattern String
  createChordProgression _  = p ""
    -- createChordProgression chordString = unzip $ unpackChordString chordString
