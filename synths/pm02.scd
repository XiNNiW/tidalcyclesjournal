
(
SynthDef(\pm02, {|freq=220, amp=1, gate=1, sustain, pan=0,out=0,harmonic=1.0,brightness=1.0|
	var smoothFreq, vibFreq, blip, mod, panner, env,noise;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	// blip = Blip.ar(lag, 10, 0.2);
	mod = PMOsc.ar(harmonic*3*vibFreq, harmonic*vibFreq*5/3, Line.ar(0,200,1), 0, 0.1);
	noise = PinkNoise.ar(EnvGen.kr(Env.perc, doneAction: 0));
	blip = PMOsc.ar(vibFreq, harmonic*0.5*vibFreq, Line.ar(20,0,4)*mod*(1+noise)*brightness, 0, 0.1);
	env = EnvGen.kr(Env.adsr, gate, timeScale:sustain, doneAction:2); // look at the helpfile for Env for more
	OffsetOut.ar(out, DirtPan.ar(blip/2, ~dirt.numChannels, pan, env));
}).add
)

// Synth(\pm02);