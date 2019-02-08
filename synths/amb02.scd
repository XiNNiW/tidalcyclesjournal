
// |out, sustain=1, pan, accelerate, freq, decay=0, slide=1, detune=1
(
SynthDef(\amb02, {
	| out = 0, attack = 0.01, decay = 0.26, sustain=0.88, release=3.5, pan = 0.5, freq, velocity=100|
	var  amp, sig;
	var envLevels = [0, 0.1, 0.88, 0.2, 0];
	var envTimes = [attack/8,attack*7/8,decay, release];
	var envLevels2 = [0, 0.62, 1, 0.5, 0];
	var envTimes2 = [attack,decay*4+attack*4, decay, release];
	var env = EnvGen.ar(Env(envLevels,envTimes, \welch), sustain, doneAction: 0);
	var env2 = EnvGen.ar(Env(envLevels2,envTimes2, \sin), 0.5, doneAction: 0);

	var lfo1 = SinOsc.ar(0.5/(attack*2),0,0.5);
	var lfo2 = SinOsc.ar(1/(decay*8),0,0.5);
	var osc1, osc2, osc3;
	osc1 = SinOsc.ar(freq+(lfo1.midicps*5), (SinOsc.ar(env*0.5/attack,0,0.5) + Sweep.ar(sustain, 2pi * [52.8, freq]) + (pi/3)).wrap(-pi, pi), [2, 0.05]);
	osc2 = SinOsc.ar(freq+(lfo1.midicps/4), (SinOsc.ar(env*0.125/attack,0,0.5)).wrap(-pi, pi), [2, 0.05]);
	osc3 = SinOsc.ar(freq/2+(lfo1.midicps/2) + (lfo2.midicps/3), (SinOsc.ar(env2,0,0.5)*osc2+osc2*env + osc1*env2/2).wrap(-pi, pi), [2, 0.05]);

	// sig = sin * env;
	// sig = RLPF(( osc3*(env)/2).tanh,freq*2,0.05);

	sig = ( osc3*(env)).tanh;

	// Out.ar(out, Pan2.ar(sig, pan, level.curvelin(0, 1, 0, 1, log(10))));
	OffsetOut.ar(out, DirtPan.ar(sig/4, ~dirt.numChannels, pan, env));
}).add;
);
