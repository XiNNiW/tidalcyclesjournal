
(
SynthDef(\pm03, {|freq=220, amp=1, gate=1, pan=0,out=0,harmonic=1.0,brightness=1.0,attack = 0.02,sustain=0.125,decay=0.2,release=0.5|
	var smoothFreq, vibFreq, osc, mod, panner, env, noise;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);


	mod = ((10000.0-smoothFreq)/10000.0)*PMOsc.ar(3*vibFreq,
		harmonic*smoothFreq*3/2,
		brightness*EnvGen.ar(Env.adsr(0.002, 0.5, 0.25, 0.25, 1, -4), gate, doneAction:0),
		0,
		0.1

	);

	noise = PinkNoise.ar(EnvGen.kr(Env.perc(0.05, 0.02, 0.5, -4), doneAction: 0));
	osc = PMOsc.ar(vibFreq/2, harmonic*0.25*smoothFreq*(1+mod*noise), brightness*Line.ar(20,0,1)*mod, 0, 0.1);
	env = EnvGen.ar(Env.adsr(attack,decay,sustain,release, 1, -4), gate, timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar(osc/2, ~dirt.numChannels, pan, env));
}).add
)

// Synth(\pm03);