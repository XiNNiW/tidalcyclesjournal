// based on https://github.com/supercollider-quarks/SynthDefPool/blob/master by danstowell
// adapted to work in tidal cycles by David Minnix
(
SynthDef(\sosbell, { |freq=440, out=0, velocity = 100, pan=0|
var son, strike, hum,output;
var amp=0.1*velocity/127.0;
// Stretched harmonic series
son = SinOsc.ar(#[2, 3, 4.1, 5.43, 6.8, 8.21] * freq, 0,
		 #[1, 0.9, 0.8, 0.7, 0.6, 0.5] * 0.1);

son = son * EnvGen.ar(Env.new([0,1,0.3, 0.2, 0], [0, 0.3, 0.3, 0.3]));

// A bit of FM adds 'warble'
son = son * LFTri.ar({Rand(1.0, 1.8)}.dup(6), 1, 0.3, 0.7);

// Mix down the partials in the main sound
son = son.mean;

strike = SinOsc.ar(LFNoise1.ar(freq * 36, 100, freq*8), 1, 0.1) *
			EnvGen.ar(Env.new([0,1,0.2, 0.1, 0], [0, 0.01, 0, 0.04]));

hum = SinOsc.ar([freq*1.01, freq*0.47], 0, EnvGen.ar(Env.new([0,0.05,0.05,0], [0.5,0.5,1]), doneAction:2)).mean;

// Out.ar(out, Pan2.ar((son + strike + hum) * 4 * amp, pan));
output = (son + strike + hum) * 4 ;
OffsetOut.ar(out, DirtPan.ar(output/2, ~dirt.numChannels, pan, amp));


},
metadata: (
	credit: "by dan stowell. based on a sound-on-sound 'synth secrets' tutorial",
	tags: [\bell, \percussion, \pitched, \additive]
	)
).add
);
