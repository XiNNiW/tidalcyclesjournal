(
SynthDef(\scream02, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,sustain,
	fb=1,indexModAttack=1.0,indexMod=1.0,
	attack=0.002,decay=0.5,sus=0.25,release=0.25
	|
	var smoothFreq, vibFreq, osc1,osc2, mod, panner, env, noise;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,1,0.007);

	mod = EnvGen.ar(Env.adsr(0.002, 0.5, 0.25, 0.25, 1, -4), gate, doneAction:0);
	osc1 = (FBSineC.ar(vibFreq, LFNoise2.kr(Lag.kr(1*indexModAttack),Lag.kr(16*indexMod),17), fb, 1.005, 0.7) * 10);
	// osc2 = BlitB3Square.ar(freq: vibFreq/*, width: 0.5, mul: 0.5, add: 0.0*/);
	env = EnvGen.ar(Env.linen(attack,decay,release, sus, 'lin'), gate, timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar((osc1).tanh, ~dirt.numChannels, pan, env));
}).add
);