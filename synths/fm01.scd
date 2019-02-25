
(
SynthDef(\fm01, {
	|freq=220, amp=1, gate=1, attack=0.02,decay=0.4, sustain, release=0.125, pan=0,out=0,harmonic=1.5,carPartial=0.5,brightness=0.5,index=3,stiffness=0.75,distance=0.5,fb=0.25,spread=0.1|
	var smoothFreq, vibFreq, oscL, oscR, mod, springMod, inforce, panner, env, noise, detune;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	detune = 1.midicps*spread/2;
	// modulation
	// mod = PMOsc.ar(harmonic*3*vibFreq, harmonic*vibFreq*5/3, Line.ar(0,200,1), 0, 0.1);
	inforce = K2A.ar(gate) > 0;
	stiffness = ControlSpec.new(minval: 0.1, maxval: 20.0, warp: 'lin').map(stiffness);//Line.kr(0.1, 20, 1);
	distance = ControlSpec.new(minval: 0.1, maxval: 0.0000001, warp:  'exp').map(distance);//Line.kr(0.00001, 0.1, 1);
	springMod = Spring.ar(inforce, stiffness, distance,0.5pi);
	// fb = ControlSpec.new(-0.5pi,0.5pi,'lin').map(fb);


	noise = PinkNoise.ar(EnvGen.kr(Env.perc(0.001,0.02), doneAction: 0));
	// mod = SinOscFB.ar(vibFreq*harmonic+springMod,springMod,1,0);
	mod = SinOsc.ar(

		vibFreq * harmonic,

		LFNoise1.kr(5.reciprocal).abs,

		vibFreq * index * springMod * LFNoise1.kr(5.reciprocal).abs + noise*index*brightness

	);
	// osc = SinOscFB.ar(vibFreq+mod*(smoothFreq/2)*brightness, springMod ,1,0).tanh;
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


// (
//
// SynthDef(\fm01, { arg bus = 0, freq = 440, carPartial = 1, modPartial = 1, index = 3, mul = 0.05,;
//
//
// 	// index values usually are between 0 and 24
//
// 	// carPartial :: modPartial => car/mod ratio
//
//
//
// 	var mod;
//
// 	var car;
//
//
//
// 	mod = SinOsc.ar(
//
// 		freq * modPartial,
//
// 		0,
//
// 		freq * index * LFNoise1.kr(5.reciprocal).abs
//
// 	);
//
//
//
// 	car = SinOsc.ar(
//
// 		(freq * carPartial) + mod,
//
// 		0,
//
// 		mul
//
// 	);
//
//
//
// 	Out.ar(
//
// 		bus,
//
// 		car
//
// 	)
//
// }).load(s);
//
// )