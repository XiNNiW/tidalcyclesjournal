// |out, sustain=1, pan, accelerate, freq, decay=0, slide=1, detune=1 |
(
SynthDef(\basicFM, {|out, sustain=1, pan, accelerate, freq, decay=0, modMult = 2, modAmount = 2000, clipAmount = 0.1|
		var modEnv = EnvGen.ar(Env.adsr(1, 0.5, 0.7, 0.1, peakLevel: modAmount), sustain);
		var mod = SinOsc.ar(modMult*freq) * modEnv;
		var car = SinOsc.ar(freq + mod);
		var ampEnv = EnvGen.ar(Env.adsr(0.1, decay, sustain, 0.2, peakLevel: 1), sustain, doneAction: 2);
		var clip = clipAmount * 100;
		var sig = (car * ampEnv * clip ).tanh;//.clip(-0.7, 0.7) * 0.1;
		OffsetOut.ar(out, DirtPan.ar(sig/2, ~dirt.numChannels, pan, 1));
	}).add;
);
