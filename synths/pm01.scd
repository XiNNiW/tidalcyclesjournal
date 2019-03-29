
(
SynthDef(\pm01, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	harmonic=1.0,brightness=1.0,overdrive=1,spread=0.01
	attack=0.1,decay=0.48,sus=1,release=0.5,sustain=1
	|
	var smoothFreq, vibFreq, oscL, oscR, panner, env,detune,mixL,mixR;
	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	detune = spread*1.midicps/2;
	overdrive = overdrive + 1;

	oscL = PMOsc.ar(
		vibFreq+detune,
		harmonic*(550/300)*smoothFreq,
		Line.ar(0,20,8)*brightness,
		0,
		overdrive
	).tanh;
	oscR = PMOsc.ar(
		vibFreq-detune,
		harmonic*(550/300)*smoothFreq,
		Line.ar(0,20,8)*brightness,
		0,
		overdrive

	).tanh;

	mixL = BLowPass.ar((oscR),freq*8*brightness);
	mixR = BLowPass.ar((oscL),freq*8*brightness);

	env = EnvGen.ar(Env.linen(attack, decay, release,sus,'lin'),timeScale:sustain, doneAction:2);
	env = env*env;
	OffsetOut.ar(out, DirtPan.ar(mixL/10, ~dirt.numChannels, pan+spread, env));
	OffsetOut.ar(out, DirtPan.ar(mixR/10, ~dirt.numChannels, pan-spread, env));
}).add
);

// Synth(\pm01);
