(
SynthDef(\scream03, {
	|
	freq=440, amp=1, gate=1,pan=0,out=0,
	attack=0.002,decay=0.49,sus=1,release=0.25, sustain,
	fb=1,indexModAttack=1.0,indexMod=1.0,
	stiffness=0.5,distance=0.15
	|
	var smoothFreq, vibFreq, osc1,osc2, mod, panner, env, noise,inforce,springMod;

	smoothFreq = Lag.kr(freq*4);
	vibFreq = Vibrato.kr(smoothFreq,1,0.02);

	inforce = K2A.ar(gate) > 0;
	stiffness = ControlSpec.new(minval: 0.1, maxval: 20.0, warp: 'lin').map(stiffness);//Line.kr(0.1, 20, 1);
	distance = ControlSpec.new(minval: 0.1, maxval: 0.0000001, warp:  'exp').map(distance);//Line.kr(0.00001, 0.1, 1);
	springMod = Spring.ar(inforce, stiffness, distance)*16;

	mod = EnvGen.ar(Env.linen(attack, decay, release, sus, 'lin'));
	osc1 = (FBSineC.ar(vibFreq,  LFNoise2.kr(Lag.kr(1*indexModAttack),Lag.kr(16*indexMod),17), fb, 1.005, 0.7)).tanh;

	env = EnvGen.ar(Env.linen(attack,decay,release,sus, 'lin'), timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar((osc1).tanh, ~dirt.numChannels, pan, env));
}).add
);

