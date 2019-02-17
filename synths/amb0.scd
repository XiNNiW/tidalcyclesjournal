
(
SynthDef(\amb0, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	attack=0.66, decay=0.2, sustain=0.8, release=1.33,
	harmonic=1.0,brightness=1.0,
	osc1tune=0, osc2tune=0,osc3tune=0,detune,spread=0
	|
	var smoothFreq, vibFreq, osc1, osc2, osc3, mod, panner, env,noise;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);

	// blip = Blip.ar(lag, 10, 0.2);
	mod = PMOsc.ar(harmonic*3*vibFreq, harmonic*vibFreq*5/3, Line.ar(0,200,1), 0, 0.1);
	noise = PinkNoise.ar(EnvGen.kr(Env.perc, doneAction: 0));
	osc1 = PMOsc.ar(vibFreq+osc1tune.midicps, harmonic*0.5*vibFreq, Line.ar(20,0,4)*mod*(1+noise)*brightness, 0, 0.1);
	osc2 = PMOsc.ar(vibFreq+osc1tune.midicps, harmonic*0.5*vibFreq, Line.ar(20,0,4)*mod*(1+noise)*brightness, 0, 0.1);
	osc3 = PMOsc.ar(vibFreq+osc1tune.midicps, harmonic*0.5*vibFreq, Line.ar(20,0,4)*mod*(1+noise)*brightness, 0, 0.1);
	env = EnvGen.kr(Env.adsr(attack,decay,1,release,amp), gate, timeScale:sustain, doneAction:2); // look at the helpfile for Env for more
	OffsetOut.ar(out, DirtPan.ar((osc1+osc2)/2, ~dirt.numChannels, pan-spread, env));
	OffsetOut.ar(out, DirtPan.ar((osc2+osc3)/2, ~dirt.numChannels, pan+spread, env));
}).add
)