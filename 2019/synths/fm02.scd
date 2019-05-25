
(
SynthDef(\fm02, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	attack = 0.0002,sus=1,decay=0.8,release=0.3, sustain=0.3,
	harmonic=1.0, brightness=1,overdrive=1,spread=0.01,muted=0.07,
	feedback=0.00088
	|
	var smoothFreq, index, op1,op2, op3,op4, env, env1,env2,env3,env4, noise, detune,strike,mix;
	var maxDelay = 30.reciprocal;

	brightness = brightness*10;

	smoothFreq = Lag.kr(freq);
	detune = spread*1.midicps/4;
	smoothFreq = smoothFreq/2;


	strike = EnvGen.ar(Env.perc(0.01,0.4), timeScale:sustain);
	strike = strike*strike*strike;
	noise = WhiteNoise.ar(1);
	feedback = feedback.tanh;
	feedback = feedback*feedback;

	env1 = EnvGen.ar(Env.perc(attack,0.25*decay), timeScale:sustain);
	env1 = env1*env1*env1*env1;
	env2 = EnvGen.ar(Env.perc(2*attack,1*decay), timeScale:sustain);
	env2 = env2*env2;
	env3 = EnvGen.ar(Env.perc(3*attack,1.333*decay), timeScale:sustain);
	env3 = env3*env3;
	env4 = EnvGen.ar(Env.perc(6*attack,1.177*decay), timeScale:sustain);
	env4 = env4*env4;

	op1 = FaustSinOperator.ar(
		K2A.ar(smoothFreq/2),
		K2A.ar(smoothFreq*brightness),
		K2A.ar(feedback*0.75*smoothFreq*4),
		env1,
		K2A.ar(0));
	 op1 = op1*WhiteNoise.ar(1);
	op2 = FaustSinOperator.ar(
		K2A.ar(smoothFreq*1.5*harmonic),
		K2A.ar(smoothFreq*brightness),
		K2A.ar(feedback*0.75*smoothFreq*4),
		env2,
		op1);
	op3 = FaustSinOperator.ar(
		K2A.ar(smoothFreq*2*harmonic),
		K2A.ar(smoothFreq*brightness),
		K2A.ar(feedback),
		env3,
		op1);
	op4 = FaustSinOperator.ar(
		K2A.ar(smoothFreq*3*harmonic),
		K2A.ar(smoothFreq*brightness),
		K2A.ar(0.01),
		env3,
		op2+op3);

	env = EnvGen.ar(Env.linen(0.0001,decay,release,sus,'lin'), gate, timeScale:sustain, doneAction:2);
	env = env*env*env;

	overdrive = overdrive + 1;
	mix = (((op4+op3+op2)*(1/3)*overdrive).tanh);
	// mix = HPF.ar(mix,7700);

	OffsetOut.ar(out, DirtPan.ar(mix, ~dirt.numChannels, pan+spread, env));
}).add
)

// Synth(\opDrm03);

