
(
SynthDef(\modal01, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	attack = 0.3,sustain=0.8,decay=0.2,release=0.5,
	harmonic=1.0,overdrive=1,spread=0.01,muted=0.007
	|
	var smoothFreq, vibFreq, oscL, oscR, env, env2, noise, detune,strike,partials,resonances,amplitudes;
	var maxDelay = 30.reciprocal;
	var fundamental = 800;
	var signalL, signalR;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	detune = spread*1.midicps/3;

	noise = WhiteNoise.ar(1);
	env = EnvGen.ar(Env.adsr(attack,decay,1,release, 1, -4), gate, timeScale:sustain, doneAction:2);
	env2 = EnvGen.ar(Env.adsr(attack*attack,decay*decay,0,0, 1, -4), gate);
	// env2 = env2*env2;
	// strike =  env2*noise/10;
	strike = Impulse.ar(0);

	partials = [fundamental, 1071, 1601, 1723];
	amplitudes = [1, 1.1, 0.9, 0.77];
	resonances = [100, 10.5, 1, 1.8];

	signalL = DynKlank.ar(`[partials, amplitudes, resonances],strike,freqscale:(freq-detune)/fundamental);
	signalR = DynKlank.ar(`[partials, amplitudes, resonances],strike,freqscale:(freq+detune)/fundamental);


	signalL = BPeakEQ.ar(signalL,233,0.8,-15);
	signalR = BPeakEQ.ar(signalR,233,0.8,-15);

	signalL = BPeakEQ.ar(signalL,4000,0.8,-25);
	signalR = BPeakEQ.ar(signalR,4000,0.8,-25);

	signalL = BPeakEQ.ar(signalL,900,0.5,-25);
	signalR = BPeakEQ.ar(signalR,900,0.5,-25);

	overdrive = overdrive + 1;
	signalL = (overdrive*(signalL/2)).tanh/2;
	signalR = (overdrive*(signalR/2)).tanh/2;

	OffsetOut.ar(out, DirtPan.ar(signalL/2, ~dirt.numChannels, pan-spread.abs, env));
	OffsetOut.ar(out, DirtPan.ar(signalR/2, ~dirt.numChannels, pan+spread.abs, env));
}).add
)

// Synth(\modal01);

// { DynKlank.ar(`[[800, 1071, 1153, 1723], nil, [1, 1, 1, 1]], Impulse.ar(2, 0, 0.1)) }.play;