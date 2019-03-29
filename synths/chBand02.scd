
(
SynthDef(\chBand02, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	attack = 0.02,sus=1,decay=0.2,release=0.15, sustain=1,
	resonance = 10.5, feedback = 10.22,
	harmonic=1.0,brightness=0.1,overdrive=1,spread=0
	|
	var smoothFreq, vibFreq, oscL, oscR, env, noise, detune, strike;
	var maxDelay = 30.reciprocal;
	var f = 800;
	var blip;
	var qmod;

	freq=freq;
	smoothFreq = Lag.kr(freq,10);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.005);
	detune = spread*1.midicps/3;

	brightness = brightness;

	noise = PinkNoise.ar(1);
	strike =  EnvGen.ar(Env.perc,gate:gate);
	strike = strike*strike;

	blip = SinOsc.ar(vibFreq);
	qmod = Pulse.ar(vibFreq/2);

	env = EnvGen.ar(Env.new([0, 1, sus,sus, 0],[attack,decay,1-release-decay-attack,release]), gate: gate, timeScale:sustain, doneAction:2);

	blip = blip*(1*smoothFreq*brightness)*(1+strike*strike);

	oscL = FaustCosChaotic2BPF.ar(
		blip,
		brightness*harmonic*qmod*K2A.ar(smoothFreq),
		K2A.ar(resonance),
		K2A.ar(feedback*Line.ar(0,1,decay+sustain*4))*(1-strike),
		qmod*brightness*Line.ar(20,1,decay+1)
	);

	oscL = HPF.ar(oscL,10);
	oscL = oscL.tanh;

	overdrive = overdrive + 1;

	OffsetOut.ar(out, DirtPan.ar((overdrive*(oscL)).tanh/2, ~dirt.numChannels, pan, env))
}).add
)

//Synth(\chBand02);
