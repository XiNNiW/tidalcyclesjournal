
(
SynthDef(\djembe, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	attack = 0.002,sus=1,decay=0.8,release=0.3, sustain=1,
	harmonic=1.0, brightness=1,overdrive=4,spread=0.01,muted=0.07,
	feedback=1
	|
	var smoothFreq, rootFreq, drum, env, noise, detune,strike,mix;
	var maxDelay = 30.reciprocal;

	smoothFreq = Lag.kr(freq);
	detune = spread*1.midicps/4;
	smoothFreq = smoothFreq/2;
	rootFreq = 85;

	strike = EnvGen.ar(Env.perc(0.01,0.4), timeScale:sustain);
	strike = strike*strike*strike;
	noise = WhiteNoise.ar(1);
	strike = strike*noise/4;

	drum = FaustDjembe.ar(strike,K2A.ar(freq));

	env = EnvGen.ar(Env.linen(0,decay,release,sus,'lin'), gate, timeScale:sustain, doneAction:2);
	env = env*env*env;

	overdrive = overdrive + 1;
	mix = ((drum)*overdrive).tanh;

	OffsetOut.ar(out, DirtPan.ar(mix, ~dirt.numChannels, pan+spread, env));
}).add
)

// Synth(\djembe);

