
(
SynthDef(\sn0w1, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	attack = 0.2,sus=1,decay=0.2,release=0.5, sustain=1,
	harmonic=1.0,brightness=5,overdrive=1,spread=0.41,muted=0.007,
	warmth=0.1,flake=0.5
	|
	var oscL, oscR,partials,resonances,amplitudes;
	var maxDelay = 30.reciprocal;
	var f = 800;
	var detune = spread*1.midicps/3;
	var smoothFreq = Lag.kr(freq);
	var vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);

	var noise = WhiteNoise.ar(1);

	var env = EnvGen.ar(Env.linen(attack,decay,release, sus, 'lin'), timeScale:sustain, doneAction:2);

	var winenv = Env([0, 1, 0], [0.5, 0.5], [8, -8]);
	var envbuf = Buffer.sendCollection(s, winenv.discretize, 1);
	var strike = EnvGen.ar(Env.linen(0.01,0,1,1));

	var grn1;
	warmth = ((warmth.abs).tanh+0.001);
	 grn1 = GrainFM.ar(2, Impulse.ar(strike*decay*500*flake), LFNoise1.kr.range(0.2,spread*0.2)*flake, vibFreq + LFNoise1.kr(12.midicps*spread), vibFreq*harmonic+(noise*spread*brightness*1.midicps), LFNoise1.kr.range(spread*(1-brightness), spread*(brightness+1)),
		pan*LFNoise1.kr.range(spread*(-1), spread), envbuf);

	overdrive = overdrive + 1;

	OffsetOut.ar(out, DirtPan.ar((overdrive*grn1).tanh/2, ~dirt.numChannels, pan, env));
}).add
)

// Synth(\sn0w1);
