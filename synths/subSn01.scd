
(
SynthDef(\subSn01, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	attack = 0.3,sus=0.8,decay=0.3,release=0.5, sustain=1,	harmonic=1.0,overdrive=0,spread=0.01,muted=0.007
	|


	var osc1, osc2, osc3, osc4, osc5, mix, env, snare;
	osc1 = BlitB3Tri.ar(freq);
	osc2 = BlitB3Tri.ar(freq+175*(111/freq));
	osc3 = BlitB3Tri.ar(111+224*(111/freq));
	osc4 = SinOsc.ar(180*(111/freq));
	osc5 = SinOsc.ar(380*(111/freq));
	snare = WhiteNoise.ar(0.75)*EnvGen.ar(Env.perc);

	mix = osc1 + osc2 + osc3 + osc4 + osc5;
	mix = ((overdrive + 1)*(mix/5+snare)).tanh/2;

	env = EnvGen.ar(Env.perc(0.001,0.75,1,'cub'), gate, timeScale:sustain, doneAction:2);

	OffsetOut.ar(out, DirtPan.ar(mix, ~dirt.numChannels, pan, env*env));
}).add
)

// Synth(\subSn01);

