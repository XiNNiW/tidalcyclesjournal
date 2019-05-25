
(
SynthDef(\opBd01, {
	|
	freq=550, amp=1, gate=1, pan=0,out=0,
	attack = 0.002,sus=1,decay=0.8,release=0.3, sustain=1,
	harmonic=1.0, brightness=1,overdrive=4,spread=0.01,muted=0.07,
	feedback=1
	|
	var smoothFreq, vibFreq, op1, op2, fcar1, fcar2, fm1, fm2, index1, index2, env, env1, env2, noise, detune,strike,mix;
	var maxDelay = 30.reciprocal;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	detune = spread*1.midicps/4;
	smoothFreq = smoothFreq/128;

	strike = EnvGen.ar(Env.perc(0.1,1), timeScale:sustain);
	strike = strike*strike*strike;
	noise = WhiteNoise.ar(1);

	env1 = EnvGen.ar(Env.perc(0.001,0.33), timeScale:sustain);
	env1 = env1*env1;
	env2 = EnvGen.ar(Env.perc(0.002,0.8*decay), timeScale:sustain);
	env2 = env2*env2*env2;

	fcar1 = K2A.ar(smoothFreq*harmonic);
	fm1 = K2A.ar(smoothFreq+250);
	index1 = FaustAntiAliasIndexScaling.ar(K2A.ar(smoothFreq+100*brightness*1),fcar1,fm1);

	op1 = FaustSinOperator.ar(
		fcar1,
		index1,
		K2A.ar(feedback),
		env1,
		SinOscFB.ar(fm1,100));

	fcar2 = K2A.ar(smoothFreq+350);
	fm2 = fcar1;
	index2 = FaustAntiAliasIndexScaling.ar(K2A.ar(smoothFreq+100*brightness*8),fcar2,fm2);

	op2 = FaustSinOperator.ar(
		fcar2,
		index2,
		K2A.ar(feedback),
		env2,
		op1);

	env = EnvGen.ar(Env.perc(0,decay*2+release), gate, timeScale:sustain, doneAction:2);
	env = env*env*env*env;

	overdrive = overdrive + 1;
	mix = ((op2*overdrive).tanh)*(strike*noise*0.58 + 1);

	OffsetOut.ar(out, DirtPan.ar(mix, ~dirt.numChannels, pan+spread, env));
}).add
)

// Synth(\opBd01);

