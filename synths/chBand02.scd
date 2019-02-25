
(
SynthDef(\chBand02, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	a = 0.02,sus=1,d=0.2,r=0.15, sustain,
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

	brightness = brightness;//*ControlSpec.new(minval: 88.midicps, maxval: 24.midicps, warp:  'lin').map(smoothFreq);

	noise = PinkNoise.ar(1);
	strike =  EnvGen.ar(Env.perc,gate:gate);//Impulse.ar(0,0,0.5);
	strike = strike*strike;

	blip = SinOsc.ar(vibFreq);
	qmod = BlitB3Square.ar(vibFreq/2);
	env = EnvGen.ar(Env.linen(a, d, r,sus,'lin'),timeScale:sustain, doneAction:2);

	// blip = LPF.ar(blip,(3*smoothFreq*brightness)*(1+strike*strike));
	//blip*env*env*env*qmod+noise*strike
	blip = blip*(1*smoothFreq*brightness)*(1+strike*strike);

	// oscL = FaustCosChaotic2BPF.ar(blip,K2A.ar(smoothFreq/2),K2A.ar(100.5)*strike,K2A.ar(9.56));
	oscL = FaustCosChaotic2BPF.ar(
		blip,
		brightness*harmonic*qmod*K2A.ar(smoothFreq),
		K2A.ar(resonance),
		K2A.ar(feedback*Line.ar(0,1,d+1))*(1-strike),
		qmod*brightness*Line.ar(20,1,d+1)
	);
/*	oscL = FaustCosChaotic2BPF.ar(
		blip,
		K2A.ar(smoothFreq),
		K2A.ar(resonance),
		K2A.ar(feedback),
		K2A.ar(0)
	);*/
	// oscL = HPF.ar(oscL,5).tanh;
	// oscL = blip;
	oscL = HPF.ar(oscL,10);
	oscL = oscL.tanh;
	// oscL = XFade2.ar(oscL,qmod,Line.ar(0,1,1));
	/*// oscR = FaustCosChaotic2BPF.ar(blip,K2A.ar(smoothFreq/2),K2A.ar(100.5)*strike,K2A.ar(9.58));
	oscR = FaustCosChaotic2BPF.ar(blip,K2A.ar(smoothFreq*harmonic*3/2-detune),K2A.ar(100),K2A.ar(1.546));
	oscR = HPF.ar(oscR,5).tanh;
	oscR = oscR - blip;
*/
	overdrive = overdrive + 1;

	OffsetOut.ar(out, DirtPan.ar((overdrive*(oscL)).tanh/2, ~dirt.numChannels, pan, env))
}).add
)

// Synth(\chBand01);

// { DynKlank.ar(`[[800, 1071, 1153, 1723], nil, [1, 1, 1, 1]], Impulse.ar(2, 0, 0.1)) }.play;