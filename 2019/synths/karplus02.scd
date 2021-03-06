
(
SynthDef(\karplus02, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	attack = 0.002,sus=1,decay=0.2,release=0.5, sustain,
	harmonic=1.0, brightness=1, atonal=1,
	overdrive=1,spread=0.01,muted=0.07
	|
	var smoothFreq, vibFreq, oscL, oscR, oscC, env, noise, detune,strike,mixL,mixR;
	var maxDelay = 30.reciprocal;
	var burst;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	detune = spread*1.midicps/4;
	atonal = atonal.tanh;
	atonal = atonal*atonal*smoothFreq;
	burst = EnvGen.kr(Env.new([0, 1, 0.8, 0.4, 0.01,0.001,0],[0,0.2,0.1,1,1,1],\exp), timeScale:sustain);

	strike = Impulse.kr(0);
	noise = WhiteNoise.ar(1);
	// oscL = Pluck.ar(noise,strike,maxDelay,(harmonic*vibFreq+detune).reciprocal,50,coef:(0-muted.tanh));
	oscL = Pluck.ar(noise,strike,maxDelay,(harmonic*vibFreq+detune*(1 + burst*LFNoise0.kr(1))).reciprocal,50,coef:(0-muted.tanh));
	noise = PinkNoise.ar(1);
	oscR = Pluck.ar(noise,strike,maxDelay,(harmonic*vibFreq-detune*(1 + burst*LFNoise0.kr(1))).reciprocal,50,coef:muted.tanh);
	noise = PinkNoise.ar(1);
	oscC = Pluck.ar(noise,strike,maxDelay,(vibFreq*0.5*(1 + burst*LFNoise0.kr(1))).reciprocal,10,coef:muted.tanh);

	env = EnvGen.ar(Env.linen(attack,decay,release,1,'lin'), timeScale:sustain, doneAction:2);

	overdrive = overdrive + 1;

	mixL = BLowPass.ar((oscC+oscR),freq*6*brightness);
	mixR = BLowPass.ar((oscC+ oscL),freq*6*brightness);

	mixL = (overdrive*(oscC+ oscL)/2).tanh;
	mixR = (overdrive*(oscC+ oscL)/2).tanh;

	OffsetOut.ar(out, DirtPan.ar(mixL/2, ~dirt.numChannels, pan-spread, env));
	OffsetOut.ar(out, DirtPan.ar(mixR/2, ~dirt.numChannels, pan+spread, env));
}).add
)

// Synth(\karplus01);

