
(
SynthDef(\stat4op, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	attack = 0.0002,decay=0.8,release=0.3,
	sustain = 1,
	harmonic=1.0, brightness=0.1,feedback=0.00088,overdrive=1,detune=0.01,
	op1ratio=1,op2ratio=2,op3ratio=2.5,op4ratio=1,
	op1level=0,op2level=0,op3level=0.88,op4level=0.88,
	op1index=1,op2index=1,op3index=0.7,op4index=0.5,
	op1attack=0.015,op2attack=0.01,op3attack=0.013,op4attack=0.001,
	op1hold=1,op2hold=0,op3hold=1,op4hold=0.33,
	op1release=0.75,op2release=1,op3release=0.6,op4release=1,
	op1feedback=0.01,op2feedback=0.1,op3feedback=0,op4feedback=0
	|

	var smoothFreq, env, noise, mix;
	var op1, op2, op3, op4;
	var op1env, op2env, op3env, op4env;

	op1level = K2A.ar(op1level);
	op2level = K2A.ar(op2level);
	op3level = K2A.ar(op3level);
	op4level = K2A.ar(op4level);

	op1ratio = harmonic*op1ratio;
	op2ratio = harmonic*op2ratio;
	op3ratio = harmonic*op3ratio;
	op4ratio = harmonic*op4ratio;

	brightness = brightness*10;
	op1index = brightness*op1index;
	op2index = brightness*op2index;
	op3index = brightness*op3index;
	op4index = brightness*op4index;

	op1feedback = feedback*op1feedback;
	op2feedback = feedback*op2feedback;
	op3feedback = feedback*op3feedback;
	op4feedback = feedback*op4feedback;

	smoothFreq = Lag.kr(freq);
	detune = detune*1.midicps/4;
	smoothFreq = smoothFreq/2;



	env = EnvGen.ar(
		Env.linen(attack,1,release,curve: \cubed),
		gate: gate,
		timeScale: sustain,
		doneAction: 2
	);

	op1env = EnvGen.ar(
		Env.linen(op1attack, op1hold, op1release, curve:\cubed),
		gate: gate,
		timeScale: sustain
	);
	op2env = EnvGen.ar(
		Env.linen(op2attack, op2hold, op2release, curve:\cubed),
		gate: gate,
		timeScale: sustain,
	);
	op3env = EnvGen.ar(
		Env.linen(op3attack, op3hold, op3release, curve:\cubed),
		gate: gate,
		timeScale: sustain,
	);
	op4env = EnvGen.ar(
		Env.linen(op4attack, op4hold, op4release,curve:\sin),
		gate: gate,
		timeScale: sustain,
	);

	env = env*env;
	op1env = op1env*op1env;
	op2env = op2env*op2env;
	op3env = op3env*op3env;
	op4env = op4env*op4env;


	op1 = FaustSinOperator.ar(
		K2A.ar(smoothFreq*op1ratio),
		K2A.ar(smoothFreq*op1index),
		K2A.ar(op1feedback*smoothFreq*op1ratio),
		op1env,
		K2A.ar(0));

	op2 = FaustSinOperator.ar(
		K2A.ar(smoothFreq*op2ratio),
		K2A.ar(smoothFreq*op2index),
		K2A.ar(op2feedback*smoothFreq*op2ratio),
		op2env,
		op1);
	op3 = FaustSinOperator.ar(
		K2A.ar(smoothFreq*op3ratio),
		K2A.ar(smoothFreq*op3index),
		K2A.ar(op3feedback*smoothFreq*op3ratio),
		op3env,
		op2);
	op4 = FaustSinOperator.ar(
		K2A.ar(smoothFreq*op4ratio),
		K2A.ar(smoothFreq*op4index),
		K2A.ar(op4feedback*smoothFreq*op4ratio),
		op4env,
		op3);


	op1 = op1*op1level;
	op2 = op2*op2level;
	op3 = op3*op3level;
	op4 = op4*op4level;

	overdrive = overdrive + 1;
	mix = ((op1+op2+op4+op3)*overdrive).tanh;
	mix = LeakDC.ar(mix);

	OffsetOut.ar(out, DirtPan.ar(mix, ~dirt.numChannels, pan, env));
}).add
)

// Synth(\stat4op);

