
(
SynthDef(\pm03, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	attack = 0.02,decay=0.125,sus=0.75,release=0.5, sustain,
	harmonic=1.0,brightness=1.0,spread =0.01,overdrive = 0
	|
	var smoothFreq, vibFreq, oscL, oscR, mod, panner, env, noise, detune;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	detune = spread*1.midicps/2;
	overdrive = overdrive + 1;


	mod = ((10000.0-smoothFreq)/10000.0)*PMOsc.ar(3*vibFreq,
		harmonic*smoothFreq*3/2,
		brightness*EnvGen.ar(Env.adsr(0.002, 0.5, 0.25, 0.25, 1, -4), gate, doneAction:0),
		0,
		0.1

	);

	noise = PinkNoise.ar(EnvGen.kr(Env.perc(0.05, 0.02, 0.5, -4), doneAction: 0));
	oscL = PMOsc.ar(vibFreq/2+detune, harmonic*0.25*smoothFreq*(1+mod*noise), brightness*Line.ar(20,0.5,decay)*mod, 0, 1);
	oscL = (overdrive*oscL).tanh;
	oscR = PMOsc.ar(vibFreq/2-detune, harmonic*0.25*smoothFreq*(1+mod*noise), brightness*Line.ar(20,0.5,decay)*mod, 0, 1);
	oscR = (overdrive*oscR).tanh;
	env = EnvGen.ar(Env.linen(attack,decay,release,sus,'lin'), timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar(oscL/3, ~dirt.numChannels, pan, env));
	OffsetOut.ar(out, DirtPan.ar(oscR/3, ~dirt.numChannels, pan, env));
}).add
)

// Synth(\pm03);