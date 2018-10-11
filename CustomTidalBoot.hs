import Sound.Tidal.MIDI.Output
import Sound.Tidal.MIDI.Context
import Sound.Tidal.MIDI.Synth


devices <- midiDevices
displayOutputDevices >>= putStrLn
m1 <- midiStream devices "VirMIDI 0-0" 1 synthController
m2 <- midiStream devices "VirMIDI 0-0" 2 synthController
m3 <- midiStream devices "VirMIDI 0-0" 3 synthController
