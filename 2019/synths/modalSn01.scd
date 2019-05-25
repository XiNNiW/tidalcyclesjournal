
(
SynthDef(\modalSn01, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	attack = 0.3,sus=0.8,decay=0.3,release=0.5, sustain=1,	harmonic=1.0,overdrive=1,spread=0.01,muted=0.007
	|

	var snareNoise,strikeNoise,strike,partials,amplitudes,resonances,body,mix,env,snare;

	snareNoise = WhiteNoise.ar(1);
	snare = EnvGen.ar(Env.perc(0.01,0.5,1,'cub'),timeScale:sustain)*snareNoise;
	strikeNoise = PinkNoise.ar(1)*EnvGen.ar(Env.perc(0,0.02,1,'cub'));

	strike = Impulse.ar(0)+strikeNoise;

	partials = [1, 1.593 , 2.135, 2.295, 2.65, 2.92, 3.16];
	amplitudes = [1, 0.93,0.88,0.88,0.88,0.88,0.75];
	resonances = [0.5, 0.5, 0.1, 0.08,0.1,0.1,0.01];

	body = DynKlank.ar(`[partials, amplitudes, resonances],strike,freqscale:harmonic*freq/2);

	mix = ((overdrive + 1)*(strikeNoise+body/4+snare/2)).tanh/2;

	env = EnvGen.ar(Env.perc, gate, timeScale:sustain, doneAction:2);

	OffsetOut.ar(out, DirtPan.ar(mix, ~dirt.numChannels, pan, env));
}).add
)

// Synth(\modalSn01);

