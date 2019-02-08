(
SynthDef(\scream03, {
	|freq=440, amp=1, gate=1, pan=0,out=0,fb=1,attack=0.2,decay=0.5,sustain=0.25,release=0.25,indexModAttack=1.0,indexMod=1.0,stiffness=0.5,distance=0.15|
	var smoothFreq, vibFreq, osc1,osc2, mod, panner, env, noise,inforce,springMod;

	smoothFreq = Lag.kr(freq*4);
	vibFreq = Vibrato.kr(smoothFreq,1,0.02);

	inforce = K2A.ar(gate) > 0;
	stiffness = ControlSpec.new(minval: 0.1, maxval: 20.0, warp: 'lin').map(stiffness);//Line.kr(0.1, 20, 1);
	distance = ControlSpec.new(minval: 0.1, maxval: 0.0000001, warp:  'exp').map(distance);//Line.kr(0.00001, 0.1, 1);
	springMod = Spring.ar(inforce, stiffness, distance)*16;

	mod = EnvGen.ar(Env.adsr(0.002, 0.5, 0.25, 0.25, 1, -4), gate, doneAction:0);
	osc1 = (FBSineC.ar(vibFreq,  LFNoise2.kr(Lag.kr(1*indexModAttack),Lag.kr(16*indexMod),17), fb, 1.005, 0.7)).tanh;

	env = EnvGen.ar(Env.adsr(attack, decay, sustain,release, 1, -4), gate, timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar((osc1).tanh/2, ~dirt.numChannels, pan, env));
}).add
);

