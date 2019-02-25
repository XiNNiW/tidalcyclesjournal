
(
SynthDef(\chBand03, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	a = 0.8,sus=0.5,d=0.2,r=0.15,sustain
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

	brightness = brightness;//*ControlSpec.new(minval: 88.midicps, maxval: 24.midicps, warp:  'lin').map(smoothFreq);

	noise = PinkNoise.ar(1);
	strike =  EnvGen.ar(Env.perc,gate:gate);//Impulse.ar(0,0,0.5);
	strike = strike*strike;

	blip = SinOsc.ar(vibFreq/2,mul:1).tanh;

	qmod = BlitB3Square.ar(vibFreq/2);
	env = EnvGen.ar(Env.linen(a, d*0.8, r*0.8,sus,'lin'),timeScale:sustain, doneAction:2);
	//env = EnvGen.ar(Env.adsr(attack,decay,1,release, 1, -4), gate, timeScale:sustain, doneAction:0);
	// blip = blip*(1+qmod*env*env*env)+noise*strike;
	blip = (blip+(blip*qmod*env*env*env))+(noise*strike*strike);

	// oscL = FaustCosChaotic2BPF.ar(blip,K2A.ar(smoothFreq/2),K2A.ar(100.5)*strike,K2A.ar(9.56));
	oscL = FaustCosChaotic2BPF.ar(
		blip,
		K2A.ar(smoothFreq),
		K2A.ar(resonance),
		K2A.ar(brightness*feedback*harmonic*smoothFreq/4),
		Line.ar(1,0,d+1)*qmod
	);
	// oscL = HPF.ar(oscL,5).tanh;

	oscL = HPF.ar(oscL,10).tanh*blip;

	oscL = XFade2.ar(oscL,qmod*(1+oscL),Line.ar(-1,1,4));
	//oscL = BPF.ar(oscL,(smoothFreq)*(brightness*strike*strike*strike+1));

	/*// oscR = FaustCosChaotic2BPF.ar(blip,K2A.ar(smoothFreq/2),K2A.ar(100.5)*strike,K2A.ar(9.58));
	oscR = FaustCosChaotic2BPF.ar(blip,K2A.ar(smoothFreq*harmonic*3/2-detune),K2A.ar(100),K2A.ar(1.546));
	oscR = HPF.ar(oscR,5).tanh;
	oscR = oscR - blip;
*/
	overdrive = overdrive + 1;

	OffsetOut.ar(out, DirtPan.ar((overdrive*(oscL*env)).tanh/3, ~dirt.numChannels, pan, env))
}).add
)

// Synth(\chBand01);

// { DynKlank.ar(`[[800, 1071, 1153, 1723], nil, [1, 1, 1, 1]], Impulse.ar(2, 0, 0.1)) }.play;