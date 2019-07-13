
(
SynthDef(\modalBd01, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	attack = 0.002,sus=1,decay=0.8,release=0.3, sustain=1,
	harmonic=1.0, brightness=1,resonance=1000,overdrive=2,
	feedback=0.12
	|


	var pulse = Impulse.ar(0)*EnvGen.ar(Env.perc(0,0.0001,1,'cub'));
	var pitchEnv = EnvGen.ar(Env.perc(0.001,0.125,freq/4,'cub'));
	var noise = WhiteNoise.ar(0.5)*EnvGen.ar(Env.perc(0.1,0.01,1,'cub'));

	var body = FaustCosChaotic2BPF.ar(
		pulse+noise,
		K2A.ar(freq/4)+pitchEnv,
		K2A.ar(resonance),
		K2A.ar(feedback),
		K2A.ar(0)
	);

	var lp = LPF.ar(pulse+noise,freq/2)+ body;

	var env = EnvGen.ar(Env.perc(0,decay,sus,'cub'), gate, timeScale:sustain, doneAction:2);

	var mix = (((lp)*(overdrive + 1)).tanh);

	OffsetOut.ar(out, DirtPan.ar(mix, ~dirt.numChannels, pan, env));
}).add
)

// Synth(\modalBd01);

