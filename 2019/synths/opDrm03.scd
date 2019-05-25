
(
SynthDef(\opDrm03, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	attack = 0.002,sus=1,decay=0.8,release=0.3, sustain=0.3,
	harmonic=1.0, brightness=1,overdrive=4,spread=0.01,muted=0.07,
	feedback=0.88
	|
	var smoothFreq, rootFreq, op1,op2, op3,op4, env, env1,env2,env3,env4, noise, detune,strike,mix;
	var maxDelay = 30.reciprocal;

	smoothFreq = Lag.kr(freq);
	detune = spread*1.midicps/4;
	smoothFreq = smoothFreq/2;
	rootFreq = 85;

	strike = EnvGen.ar(Env.perc(0.01,0.4), timeScale:sustain);
	strike = strike*strike*strike;
	noise = WhiteNoise.ar(1);
	feedback = feedback.tanh;
	feedback = feedback*feedback;

	env1 = EnvGen.ar(Env.perc(0.0002,0.125), timeScale:sustain);
	env1 = env1*env1*env1*env1;
	env2 = EnvGen.ar(Env.perc(0.002,0.5*decay), timeScale:sustain);
	env2 = env2*env2;
	env3 = EnvGen.ar(Env.perc(0.003,0.333*decay), timeScale:sustain);
	env3 = env3*env3;
	env4 = EnvGen.ar(Env.perc(0.03,0.177*decay), timeScale:sustain);
	env4 = env4*env4;

	op1 = FaustSinOperator.ar(
		K2A.ar(smoothFreq),
		K2A.ar(smoothFreq*brightness),
		K2A.ar(feedback*0.75*smoothFreq*4),
		env1,
		K2A.ar(0));
	op1 = op1*WhiteNoise.ar(1);
	op2 = FaustSinOperator.ar(
		K2A.ar(smoothFreq*(7150/rootFreq)),
		K2A.ar(smoothFreq*brightness*100),
		K2A.ar(feedback*0.75*smoothFreq*4),
		env2,
		op1);
	op3 = FaustSinOperator.ar(
		K2A.ar(smoothFreq*(3433/rootFreq)),
		K2A.ar(smoothFreq*brightness*100),
		K2A.ar(feedback),
		env3,
		op1);
	op4 = FaustSinOperator.ar(
		K2A.ar(smoothFreq*(1400/rootFreq)),
		K2A.ar(smoothFreq*brightness*100),
		K2A.ar(0.01),
		env3,
		op2+op3);

	env = EnvGen.ar(Env.linen(0.0001,decay,release,sus,'lin'), gate, timeScale:sustain, doneAction:2);
	env = env*env*env;

	overdrive = overdrive + 1;
	mix = (((op4+op3+op2)*overdrive).tanh);
	mix = HPF.ar(mix,7700);

	OffsetOut.ar(out, DirtPan.ar(mix, ~dirt.numChannels, pan+spread, env));
}).add
)

// Synth(\opDrm03);

