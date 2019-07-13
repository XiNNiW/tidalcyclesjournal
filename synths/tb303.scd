(
var tb303bus = Bus.control(s,7);

SynthDef(\tb303c, {|n=0, gain=1, pan=0.5, speed, cutoff, q, sustain|
	Out.kr(tb303bus, [n, gain, pan, speed, cutoff, q, sustain]);
}).add;

SynthDef(\tb303, {|out, pan, freq, sustain|
	var sound, env, fenv, lfreq, lgain, lpan, lag, locals;
	env = EnvGen.ar(Env.linen(0.005, 0.99, 0.005), timeScale:sustain, doneAction:2);
	locals = In.kr(tb303bus, 7);

	fenv = EnvGen.ar(Env.perc(0.02, 2, 1, [1,-1]), gate:Changed.kr(locals[0]), timeScale:locals[6]);
	lag = 0.5 * locals[6] / locals[3];
	lfreq = freq * Lag.kr(2.0 ** (locals[0]/12), lag);
	lgain = 1.0 * Lag.kr(locals[1], lag);
	lpan = pan + Lag.kr(locals[2], lag);
	sound = Splay.ar(VarSaw.ar(lfreq*[0.99,1.01], [0,0.2], 0.98), 0.4, levelComp:false);
	sound = DFM1.ar(sound, freq*locals[4]*fenv+locals[4],locals[5]);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, lpan, env*lgain));
}).add;
)