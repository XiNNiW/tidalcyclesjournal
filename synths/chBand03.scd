
(
SynthDef(\chBand03, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	a = 0.8,sus=0.5,d=0.2,r=0.15,sustain=1,
	resonance = 100, feedback = -0.125,
	harmonic=3.0,brightness=0.1,overdrive=1,spread=0
	|
	var smoothFreq, vibFreq, oscL, oscR, env, noise, detune, strike;
	var maxDelay = 30.reciprocal;
	var f = 800;
	var blip;
	var qmod;

	freq=freq;
	smoothFreq = Lag.kr(freq,100);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.005);
	detune = spread*1.midicps/3;

	brightness = brightness;

	noise = PinkNoise.ar(1);
	strike =  EnvGen.ar(Env.perc,gate:gate);
	strike = strike*strike;

	blip = SinOsc.ar(vibFreq/2,mul:1).tanh;

	qmod = BlitB3Square.ar(vibFreq/2);
	env = EnvGen.ar(Env.linen(a, d*0.8, r*0.8,sus,'lin'),timeScale:sustain, doneAction:2);

	blip = (blip+(blip*qmod*env*env*env))+(noise*strike*strike);

	oscL = FaustCosChaotic2BPF.ar(
		blip,
		K2A.ar(smoothFreq),
		K2A.ar(resonance),
		K2A.ar(brightness*feedback*harmonic*smoothFreq/4),
		Line.ar(1,0,d+1)*qmod
	);

	oscL = HPF.ar(oscL,10).tanh*blip;

	// oscL = XFade2.ar(oscL,qmod*(1+oscL),Line.ar(-1,1,4));

	overdrive = overdrive + 1;

	OffsetOut.ar(out, DirtPan.ar((overdrive*(oscL*env)).tanh/3, ~dirt.numChannels, pan, env))
}).add
)

// Synth(\chBand03);
