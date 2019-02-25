
(
SynthDef(\pm02, {
	|
	freq=220, amp=1, gate=1,pan=0,out=0,
	attack=0.01,decay=0.3,sus=1,release=1, sustain,
	harmonic=1.0,brightness=1.0,spread=0.01,overdrive=0
	|
	var smoothFreq, vibFreq, blipL,blipR, mod, panner, env,noise;
	var detune =spread*1.midicps/2;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);

	mod = PMOsc.ar(harmonic*3*vibFreq, harmonic*vibFreq*5/3, Line.ar(0,200,1), 0, 0.1);
	noise = PinkNoise.ar(EnvGen.kr(Env.perc, doneAction: 0));

	overdrive = 1 +overdrive;
	blipL = PMOsc.ar(vibFreq+detune, harmonic*0.5*vibFreq, Line.ar(20,0,decay)*mod*(1+noise)*brightness, 0, 0.1,mul:overdrive);
	blipL = blipL.tanh/2;
	blipR = PMOsc.ar(vibFreq-detune, harmonic*0.5*vibFreq, Line.ar(20,0,decay)*mod*(1+noise)*brightness, 0, 0.1,mul:overdrive);
	blipR = blipR.tanh/2;

	env = EnvGen.kr(Env.linen(attack,decay,release,sus,'lin'), gate, timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar(blipL/5, ~dirt.numChannels, pan, env));
	OffsetOut.ar(out, DirtPan.ar(blipR/5, ~dirt.numChannels, pan, env));
}).add
)

// Synth(\pm02);