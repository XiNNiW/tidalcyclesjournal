
(
SynthDef(\fmSnare01, {
	|
	freq=220,amp=1,gate=1,pan=0,out=0,
	attack=0,decay=0.2,sustain,release,
	harmonic=1.11,carPartial=0.5,brightness=1,index=5,
	stiffness=0.75,distance=0.35
	|
	var smoothFreq, osc, mod, springMod, inforce, panner, env, noise;

	smoothFreq = Lag.kr(freq);
	// vibFreq = Vibrato.ar(smoothFreq,0.5,0.01);
	// modulation

	inforce = K2A.ar(gate) > 0;
	stiffness = ControlSpec.new(minval: 0.1, maxval: 20.0, warp: 'lin').map(stiffness);
	distance = ControlSpec.new(minval: 0.1, maxval: 0.0000001, warp:  'exp').map(distance);
	springMod = Spring.ar(inforce, stiffness, distance,1);

	noise = PinkNoise.ar(EnvGen.kr(Env.perc(0.001,(0.2+1)*decay), doneAction: 0));
	mod = SinOsc.ar(
		smoothFreq * harmonic,
		LFNoise1.kr(5.reciprocal).abs,
		smoothFreq * index * springMod + smoothFreq * index * noise * brightness
	);
	osc = SinOsc.ar(
		(smoothFreq * carPartial*springMod) + mod+noise*smoothFreq*(index/100)*amp*brightness*springMod,
		LFNoise1.kr(5.reciprocal).abs,
		springMod
	).tanh;
	env = EnvGen.kr(Env.adsr(attack,decay,1,0.125,1,-4), gate, timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar((osc+noise)/2, ~dirt.numChannels, pan, env));
}).add
);

// Synth(\fmSnare01);