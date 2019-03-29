
(
SynthDef(\opDrm02, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	attack = 0.002,sus=1,decay=0.8,release=0.3, sustain=0.3,
	harmonic=1.0, brightness=1,overdrive=4,spread=0.01,muted=0.07,
	feedback=1.5
	|
	var smoothFreq, rootFreq, op1,op2, op3, fcar1,fcar2,fcar3,fm1,fm2,fm3, index1,index2,index3, env, env1,env2,env3, noise, detune,strike,mix;
	var maxDelay = 30.reciprocal;

	smoothFreq = Lag.kr(freq);
	detune = spread*1.midicps/4;
	smoothFreq = smoothFreq/2;
	rootFreq = 85;

	strike = EnvGen.ar(Env.perc(0.01,2), timeScale:sustain);
	strike = strike*strike*strike;
	noise = WhiteNoise.ar(1);



	env1 = EnvGen.ar(Env.perc(0.0002,1), timeScale:sustain);
	env1 = env1*env1;
	env2 = EnvGen.ar(Env.perc(0.002,0.7*decay), timeScale:sustain);
	env2 = env2*env2*env2;
	env3 = EnvGen.ar(Env.perc(0.003,0.7*decay), timeScale:sustain);
	env3 = env3*env3*env3;

	fcar1 = Line.ar(2450,smoothFreq,0.015);
	fm1 = K2A.ar(8000);
	index1 = FaustAntiAliasIndexScaling.ar(K2A.ar(smoothFreq*brightness),fcar1,fm1);

	op1 = FaustSinOperator.ar(
		fcar1,
		index1,
		K2A.ar(feedback),
		env1,
		(strike*noise ));

	fcar2 = K2A.ar(smoothFreq*(243/rootFreq));
	fm2 = fcar1;
	index2 = FaustAntiAliasIndexScaling.ar(K2A.ar(smoothFreq*brightness*30),fcar2,fm2);
	op2 = FaustSinOperator.ar(
		fcar2,
		index2,
		K2A.ar(feedback*0.75),
		env2,
		op1);

	fcar3 = K2A.ar(smoothFreq*(433/rootFreq));
	fm3 = fcar1;
	index3 = FaustAntiAliasIndexScaling.ar(K2A.ar(smoothFreq*brightness*10),fcar2,fm2);
	op3 = FaustSinOperator.ar(
		fcar3,
		index3,
		K2A.ar(feedback*0.5),
		env3,
		op1);

	env = EnvGen.ar(Env.linen(0,decay,release,sus,'lin'), gate, timeScale:sustain, doneAction:2);
	env = env*env*env*env;

	overdrive = overdrive + 1;
	mix = (((op2+op3+op1)*overdrive).tanh);

	OffsetOut.ar(out, DirtPan.ar(mix, ~dirt.numChannels, pan+spread, env));
}).add
)

// Synth(\opDrm02);

