
(
SynthDef(\modal03, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	a = 0.2,sus=1,d=0.4,r=0.5,sustain=1,
	harmonic=1.0,overdrive=0,spread=1,muted=0.007
	|
	var smoothFreq, vibFreq, oscL, oscR, env, noise, detune,strike,partials,resonances,amplitudes;
	var maxDelay = 30.reciprocal;
	var f = 800;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	detune = spread*1.midicps/3;

	noise = WhiteNoise.ar(1);
	strike =  EnvGen.ar(Env.perc);

	partials = [f, f*4, f*3, f*8.33];
	amplitudes = [1, 1.1, 0.9, 1.5];
	resonances = [5, 0.22, 1, 0.08];

	oscL = DynKlank.ar(`[partials, amplitudes, resonances],strike,freqscale:(freq-detune)/f);

	oscR = DynKlank.ar(`[partials, amplitudes, resonances],strike,freqscale:(freq+detune)/f);

	env = EnvGen.ar(Env.linen(a,d,r, sus, 'lin'), timeScale:sustain, doneAction:2);

	overdrive = overdrive + 1;

	OffsetOut.ar(out, DirtPan.ar((overdrive*(oscL)).tanh/10, ~dirt.numChannels, pan-spread.abs, env));
	OffsetOut.ar(out, DirtPan.ar((overdrive*(oscR)).tanh/10, ~dirt.numChannels, pan+spread.abs, env));
}).add
)

// Synth(\modal03);

// { DynKlank.ar(`[[800, 1071, 1153, 1723], nil, [1, 1, 1, 1]], Impulse.ar(2, 0, 0.1)) }.play;