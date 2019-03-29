
(
SynthDef(\fm01, {
	|
	freq=220, amp=1, gate=1, attack=0.02,decay=0.4, sustain=1, release=0.125, pan=0,out=0,harmonic=1.5,carPartial=0.5,brightness=0.5,index=3,stiffness=0.75,distance=0.5,fb=0.25,spread=0.1
	|
	var smoothFreq, vibFreq, oscL, oscR, mod, springMod, inforce, panner, env, noise, detune;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	detune = 1.midicps*spread/2;
	inforce = K2A.ar(gate) > 0;
	stiffness = ControlSpec.new(minval: 0.1, maxval: 20.0, warp: 'lin').map(stiffness);
	distance = ControlSpec.new(minval: 0.1, maxval: 0.0000001, warp:  'exp').map(distance);
	springMod = Spring.ar(inforce, stiffness, distance,0.5pi);

	noise = PinkNoise.ar(EnvGen.kr(Env.perc(0.001,0.02), doneAction: 0));

	mod = SinOsc.ar(

		vibFreq * harmonic,

		LFNoise1.kr(5.reciprocal).abs,

		vibFreq * index * springMod * LFNoise1.kr(5.reciprocal).abs + noise*index*brightness

	);

	oscL = SinOsc.ar(

		(vibFreq * carPartial) + mod - detune,

		LFNoise1.kr(5.reciprocal).abs,

		springMod

	);
	oscR = SinOsc.ar(

		(vibFreq * carPartial) + mod + detune,

		LFNoise1.kr(5.reciprocal).abs,

		springMod

	);
	env = EnvGen.kr(Env.linen(attack,0.3,release,1,'lin'),gate, timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar((oscL.tanh)/2, ~dirt.numChannels, pan+spread, env*env));
	OffsetOut.ar(out, DirtPan.ar((oscR.tanh)/2, ~dirt.numChannels, pan-spread, env*env));
}).add
);

// Synth(\fm01);

