(
var dronebus = Bus.control(s,5);

SynthDef(\dronec, {|n=0, gain=1, pan=0.5, speed, sustain|
	Out.kr(dronebus, [n, gain, pan, speed, sustain]);
}).add;

SynthDef(\drone, {|out, pan, freq, sustain|
	var sound, env, fenv, lfreq, lgain, lpan, lag, locals;
	env = EnvGen.ar(Env.linen(0.005, 0.99, 0.005), timeScale:sustain, doneAction:2);
	locals = In.kr(dronebus, 5);
	fenv = EnvGen.ar(Env.perc(0.02, 2, 1, [1,-1]), gate:Changed.kr(locals[0]), timeScale:locals[4]);
	lag = 0.5 * locals[4] / locals[3];
	lfreq = freq * Lag.kr(2.0 ** (locals[0]/12), lag);
	lgain = 1.0 * Lag.kr(locals[1], lag);
	lpan = pan + Lag.kr(locals[2], lag);
	sound = Splay.ar(VarSaw.ar(lfreq*[0.99,1.01], [0,0.2], 0.98), 0.4, levelComp:false);
	sound = LPF.ar(sound, freq*10*fenv+800);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, lpan, env*lgain));
}).add;
)