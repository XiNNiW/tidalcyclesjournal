// based on https://github.com/brunoruviaro/SynthDefs-for-Patterns/blob/master by  brunoruviaro
// adapted for use in tidalcycles by david Minnix
(
SynthDef(\marimba1, {| freq = 440, velocity = 100, pan = 0, out=0|
	var snd, env;
	var amp = velocity/127.0;
	env = Env.linen(0.015, 1, 0.5, amp).kr(doneAction: 2);
	snd = BPF.ar(Saw.ar(0), freq, 0.02);
	snd = BLowShelf.ar(snd, 220, 0.81, 6);
	snd = snd * env;
	// Out.ar(0, Splay.ar(snd));
	OffsetOut.ar(out, DirtPan.ar(Splay.ar(snd)/2, ~dirt.numChannels, pan, 1));

}).add;
)

// (
// Pbind(
// 	\instrument, "marimba1",
// 	\degree, Pwhite(-10, 10),
// 	\dur, Pwhite(0.2, 0.3),
// 	\amp, Pwhite(0.1, 0.5)
// ).play;
// )
