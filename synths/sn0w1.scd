
(
SynthDef(\sn0w1, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	attack = 0.2,sustain=1,decay=0.2,release=0.5,
	harmonic=1.0,brightness=5,overdrive=0,spread=0.41,muted=0.007,
	warmth=0.1,flake=0.5
	|
	var oscL, oscR,partials,resonances,amplitudes;
	var maxDelay = 30.reciprocal;
	var f = 800;
	var detune = spread*1.midicps/3;
	var smoothFreq = Lag.kr(freq);
	var vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);

	var noise = WhiteNoise.ar(1);

	var env = EnvGen.ar(Env.adsr(attack,decay,1,release, 1, -4), gate, timeScale:sustain, doneAction:2);

	var winenv = Env([0, 1, 0], [0.5, 0.5], [8, -8]);
	var envbuf = Buffer.sendCollection(s, winenv.discretize, 1);
	var strike = EnvGen.ar(Env.linen(0.01,0,1,1));


	var grn1;
	warmth = ((warmth.abs).tanh+0.001);
	 grn1 = GrainFM.ar(2, Impulse.ar(strike*decay*500*flake), LFNoise1.kr.range(0.2,spread*0.2)*flake, vibFreq + LFNoise1.kr(12.midicps*spread), vibFreq*harmonic+(noise*spread*brightness*1.midicps), LFNoise1.kr.range(spread*(1-brightness), spread*(brightness+1)),
		pan*LFNoise1.kr.range(spread*(-1), spread), envbuf);

	grn1 = BPF.ar(in: grn1, freq: smoothFreq*(1+3*brightness), rq: warmth, mul: 1.0, add: 0.0);





	overdrive = overdrive + 1;

	OffsetOut.ar(out, DirtPan.ar((overdrive*grn1).tanh/2, ~dirt.numChannels, pan, env));
}).add
)

// Synth(\modal01);

// { DynKlank.ar(`[[800, 1071, 1153, 1723], nil, [1, 1, 1, 1]], Impulse.ar(2, 0, 0.1)) }.play;