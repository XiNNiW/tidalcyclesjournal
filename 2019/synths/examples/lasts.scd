(
	SynthDef(\lasts, {|freq = 440, pan=0, velocity=100, gate = 1,out=0|
		var env = EnvGen.ar(
			envelope: Env.adsr(1, 0.5, 0.5, 0.8, 1, -4,0),
			gate: gate,
			doneAction: 2
			);
		var sig = (SinOsc.ar(freq,0,0.5, 1)+SinOsc.ar(3*freq/2,0,0.5, 1)).tanh;
		OffsetOut.ar(out, DirtPan.ar(sig/2, ~dirt.numChannels, pan, env));
	}).add
);

// Synth(\lasts).play()
