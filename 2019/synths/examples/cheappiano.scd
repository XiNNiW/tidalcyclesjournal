// based on https:github.com/supercollider-quarks/SynthDefPool/blob/master/pool/cheappiano.scd
// adapted for use in tidal cycles by david minnix 2018

(
SynthDef(\cheappiano, { | out=0, freq=440, velocity=100, dur=1, pan=0 |
	var sig, in, n = 6, max = 0.04, min = 0.01, delay, pitch, detune, hammer;
	var amp = 0.1*velocity/127.0;
	freq = freq.cpsmidi;
	hammer = Decay2.ar(Impulse.ar(0.001), 0.008, 0.04, LFNoise2.ar([2000,4000].asSpec.map(amp), 0.25));
	sig = Mix.ar(Array.fill(3, { arg i;
			detune = #[-0.04, 0, 0.03].at(i);
			delay = (1/(freq + detune).midicps);
			CombL.ar(hammer, delay, delay, 50 * amp)
		}) );

	sig = HPF.ar(sig,50) * EnvGen.ar(Env.perc(0.0001,dur, amp * 4, -1), doneAction:2);
	// Out.ar(out, Pan2.ar(sig, pan));
	OffsetOut.ar(out, DirtPan.ar(sig/2, ~dirt.numChannels, pan, 1));

},
metadata: (
	credit: "based on something posted 2008-06-17 by jeff, based on an old example by james mcc",
	tags: [\casio, \piano, \pitched]
	)
).add
);
