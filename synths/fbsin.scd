
(
var fbsinControlBus = Bus.control(s,7);
SynthDef(\fbsinc, {
	|
	sustain, feedback, brightness, q, saturation,
	vibDepth,vibRate
	|
	Out.kr(fbsinControlBus,[sustain,feedback, brightness,q,saturation,vibDepth,vibRate]);

}).add;

SynthDef(\fbsin, {
	|
	freq=220, amp=1, gate=1, pan=0, out=0,
	feedback=1.11, brightness=5.0, q = 0.12,
	attack=0.0002, decay=0.2, sus=1, release=0.5, sustain=1,
	vibDepth=0.02,vibRate=1,
	saturation=0.5
	|
	var env, env2, signal, externalControls;
	externalControls = In.kr(fbsinControlBus, 7);

	feedback = feedback + Lag.kr(externalControls[1]);
	brightness = brightness + Lag.kr(externalControls[2]);
	q = q + Lag.kr(externalControls[3]);
	saturation = saturation + Lag.kr(externalControls[4]);
	vibDepth = vibDepth + Lag.kr(externalControls[5]);
	vibRate = vibRate + Lag.kr(externalControls[6]);

	freq = Lag.kr(freq);
	freq = Vibrato.kr(freq, vibRate, vibDepth);

	env = EnvGen.ar(
		Env.linen(attack, decay, release, sus, 'lin'),
		gate,
		timeScale:sustain,
		doneAction:2
	);
	env=env*env;
	env2 = EnvGen.ar(
		Env.linen(attack*0.1, decay*0.1, release, sus, 'lin'),
		gate,
		timeScale:sustain+ externalControls[0],
		doneAction:2
	);
	env2=env2*env2;

	feedback = feedback*pi*env2;
	saturation = saturation + 1;
	signal = FBSineC.ar(freq,1,feedback,1.1,0.5,0.1,0.1,saturation,0.0).tanh;
	signal = BMoog.ar(
		in:signal/2,
		freq:freq*brightness,
		q:q,
		mode: 0,
		saturation: saturation
	);

	OffsetOut.ar(out, DirtPan.ar(signal/2, ~dirt.numChannels, pan, env));
}).add;
)

// Synth(\fbsin);