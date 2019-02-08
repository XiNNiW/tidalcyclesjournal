//from http://sccode.org/1-5aW
// |out, sustain=1, pan, accelerate, freq, decay=0, slide=1, detune=1
(
SynthDef(\ikedabass, { | out = 0, sustain=1, pan = 0, accelerate, freq, decay=0,  level = 1|
	var env, sin, sig;
	env = EnvGen.ar(Env([0, 0.5, 0.4, 0], [0, 0.2, 0.01], -5), sustain, doneAction: 0);
	sin = SinOsc.ar(0, (Sweep.ar(sustain, 2pi * [52.8, freq]) + (pi/3)).wrap(-pi, pi), [2, 0.05]).mean.tanh;
	// sig = sin * env;
	sig = sin;
	// Out.ar(out, Pan2.ar(sig, pan, level.curvelin(0, 1, 0, 1, log(10))));
	OffsetOut.ar(out, DirtPan.ar(sig/2, ~dirt.numChannels, pan, env));
}).add;
);
