
(
SynthDef(\modal02, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	attack = 0.2,sustain=1,decay=0.2,release=0.5,
	harmonic=1.0,overdrive=10,spread=1,muted=0.007
	|
	var smoothFreq, vibFreq, oscL, oscR, env, noise, detune,strike,partials,resonances,amplitudes;
	var maxDelay = 30.reciprocal;
	var signalL, signalR;
	var f = 800;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	detune = spread*1.midicps/3;

	noise = WhiteNoise.ar(1);
	strike =  Impulse.ar(0, 0, 0.3)*noise*1;

	partials = [f, f*4, f*3, f*8.33];
	amplitudes = [1, 1.1, 0.9, 1.5];
	resonances = [10, 0.22, 1, 0.08];

	signalL = DynKlank.ar(`[partials, amplitudes, resonances],strike,freqscale:(freq-detune)/f);

	signalR = DynKlank.ar(`[partials, amplitudes, resonances],strike,freqscale:(freq+detune)/f);

	env = EnvGen.ar(Env.adsr(attack,decay,1,release), gate, timeScale:sustain, doneAction:2);


	signalL = BPeakEQ.ar(signalL,233,0.8,-15);
	signalR = BPeakEQ.ar(signalR,233,0.8,-15);

	signalL = BPeakEQ.ar(signalL,4000,0.8,-25);
	signalR = BPeakEQ.ar(signalR,4000,0.8,-25);

	signalL = BPeakEQ.ar(signalL,900,0.5,-25);
	signalR = BPeakEQ.ar(signalR,900,0.5,-25);

	overdrive = overdrive + 1;
	signalL = (overdrive*(signalL)).tanh/2;
	signalR = (overdrive*(signalR)).tanh/2;

	OffsetOut.ar(out, DirtPan.ar(signalL/2, ~dirt.numChannels, pan-spread.abs, env));
	OffsetOut.ar(out, DirtPan.ar(signalR/2, ~dirt.numChannels, pan+spread.abs, env));
}).add
)

// Synth(\modal01);

// { DynKlank.ar(`[[800, 1071, 1153, 1723], nil, [1, 1, 1, 1]], Impulse.ar(2, 0, 0.1)) }.play;