(
SynthDef(\scream02, {
	|freq=220, amp=1, gate=1, pan=0,out=0,fb=1,attack=0.002,decay=0.5,sustain=0.25,release=0.25,indexModAttack=1.0,indexMod=1.0|
	var smoothFreq, vibFreq, osc1,osc2, mod, panner, env, noise;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,1,0.02);

	mod = EnvGen.ar(Env.adsr(0.002, 0.5, 0.25, 0.25, 1, -4), gate, doneAction:0);
	osc1 = (FBSineC.ar(vibFreq, LFNoise2.kr(Lag.kr(1*indexModAttack),Lag.kr(16*indexMod),17), fb, 1.005, 0.7) * 0.2);
	osc2 = 0;//Pulse.ar(freq: vibFreq/2, width: 0.25, mul: 1.0, add: 0.0)/3;
	env = EnvGen.ar(Env.adsr(0.02, 0.2, 0.125, 0.5, 1, -4), gate, timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar((osc1+osc2).tanh/2, ~dirt.numChannels, pan, env));
}).add
);