
(
SynthDef(\modal01, {
	|
	freq=440, amp=1, gate=1, pan=0,out=0,
	a = 0.3,sus=0.8,d=0.3,r=0.5, sustain=1,
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
	env = EnvGen.ar(Env.linen(a,d,r,sus, 'lin'), gate, timeScale:sustain, doneAction:2);
	env2 = EnvGen.ar(Env.adsr(a*a,d*d,0,0, 1, -4), gate);

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

