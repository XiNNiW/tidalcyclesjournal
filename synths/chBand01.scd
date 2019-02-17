
(
SynthDef(\chBand01, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	attack = 0.2,sustain=1,decay=0.2,release=0.15,
	resonance = 100, feedback = 100.25,
	harmonic=1.0,brightness=1.0,overdrive=0,spread=0,muted=0.007
	|
	var smoothFreq, vibFreq, oscL, oscR, env, noise, detune,strike,partials,resonances,amplitudes;
	var maxDelay = 30.reciprocal;
	var f = 800;
	var blip;
	var qmod;

	freq=freq;
	smoothFreq = Lag.kr(freq,100);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	detune = spread*1.midicps/3;

	noise = PinkNoise.ar(1);
	strike =  EnvGen.ar(Env.perc,gate:gate);//Impulse.ar(0,0,0.5);
	strike = strike*strike;

	partials = [f, f*4, f*3, f*8.33];
	amplitudes = [1, 1.1, 0.9, 1.5];
	resonances = [5, 0.22, 1, 0.08];

	blip = BlitB3Saw.ar(vibFreq);
	qmod = SinOsc.ar(smoothFreq);
	env = EnvGen.ar(Env.adsr(attack,decay,1,release, 1, -4), gate, timeScale:sustain, doneAction:2);
	blip = blip.tanh*env*env*env*env;
	blip = LPF.ar(blip,(smoothFreq)*(10*brightness*strike*strike*strike+1));

	// oscL = FaustCosChaotic2BPF.ar(blip,K2A.ar(smoothFreq/2),K2A.ar(100.5)*strike,K2A.ar(9.56));
	oscL = FaustCosChaotic2BPF.ar(blip,brightness*(40/100)*qmod*K2A.ar(smoothFreq/4) + K2A.ar(smoothFreq*harmonic*2-detune),K2A.ar(resonance),K2A.ar(feedback)*(1-strike));
	// oscL = HPF.ar(oscL,5).tanh;
	oscL =  oscL;
	oscL = HPF.ar(oscL,1).tanh;
	/*// oscR = FaustCosChaotic2BPF.ar(blip,K2A.ar(smoothFreq/2),K2A.ar(100.5)*strike,K2A.ar(9.58));
	oscR = FaustCosChaotic2BPF.ar(blip,K2A.ar(smoothFreq*harmonic*3/2-detune),K2A.ar(100),K2A.ar(1.546));
	oscR = HPF.ar(oscR,5).tanh;
	oscR = oscR - blip;
*/
	overdrive = overdrive + 1;

	OffsetOut.ar(out, DirtPan.ar((overdrive*(oscL)).tanh/4, ~dirt.numChannels, pan-spread.abs, env));
	OffsetOut.ar(out, DirtPan.ar((overdrive*(oscL)).tanh/4, ~dirt.numChannels, pan+spread.abs, env));
}).add
)

// Synth(\chBand01);

// { DynKlank.ar(`[[800, 1071, 1153, 1723], nil, [1, 1, 1, 1]], Impulse.ar(2, 0, 0.1)) }.play;