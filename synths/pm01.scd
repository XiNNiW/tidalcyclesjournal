
(
SynthDef(\pm01, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	harmonic=1.0,brightness=1.0,drive=1,spread=0.01
	attack,decay,sustain,release
	|
	var smoothFreq, vibFreq, oscL, oscR, panner, env,detune,mixL,mixR;
	// {sustain.value}.value().postln;
	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	detune = spread*1.midicps/2;

	oscL = PMOsc.ar(vibFreq+detune, harmonic*(550/300)*smoothFreq, Line.ar(0,20,8)*brightness, 0, 1+drive).tanh;
	oscR = PMOsc.ar(vibFreq-detune, harmonic*(550/300)*smoothFreq, Line.ar(0,20,8)*brightness, 0, 1+drive).tanh;

	mixL = BLowPass.ar((oscR),freq*8*brightness);
	mixR = BLowPass.ar((oscL),freq*8*brightness);

	env = EnvGen.ar(Env.adsr(attack,decay,1,release), gate, timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar(mixL/10, ~dirt.numChannels, pan+spread, env));
	OffsetOut.ar(out, DirtPan.ar(mixR/10, ~dirt.numChannels, pan-spread, env));
}).add
);

// Synth(\pm01);
