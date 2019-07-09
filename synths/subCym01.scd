
(
SynthDef(\subCym01, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	attack = 0.02,sus=1,decay=0.2,release=0.15, sustain=4,
	resonance = 10.5, feedback = 10.22,
	harmonic=2.33,brightness=0.1,overdrive=1,spread=0
	|

	var noise = PinkNoise.ar(1);
	var pulses = Pulse.ar(
		[freq*(1.37),freq*(-1.22),freq*(1.411),freq*(-1.054),freq*(1.033),freq]
	);
	var env1 = EnvGen.ar(Env.perc(0.01,1,curve:'cub'),timeScale:sustain);
	var env2 = EnvGen.ar(Env.perc(0.0002,0.1175,curve:'cub'),timeScale:sustain);
	var env3 = EnvGen.ar(Env.perc(0.002,0.125,curve:'cub'),timeScale:sustain);
	var bp1 = BPF.ar(pulses,8000)*env1;
	var bp2 = BPF.ar(pulses+noise,3000);
	var highs = HPF.ar(bp1,8500)*env1;
	var mids = HPF.ar(bp1,7000)*env2;
	var lows = HPF.ar(bp2,2000)*env3;

	var env = EnvGen.ar(
		Env.perc(0.0001,1,curve:'cub'),
		gate:gate,timeScale:sustain,
		doneAction:2
	);
	var mix = highs+mids*0.68+lows;

	OffsetOut.ar(
		out,
		DirtPan.ar(
			((overdrive+1)*(mix)).tanh,
			~dirt.numChannels,
			pan,
			env*env
		)
	)
}).add
)

// Synth(\subCym01);
