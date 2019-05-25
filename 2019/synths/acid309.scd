// based on https://github.com/supercollider-quarks/SynthDefPool/blob/master by danstowell
// adapted to work in tidal cycles by David Minnix
(
SynthDef("acid309", {
	| out=0, gate=1, freq=440, velocity=100, pan=0|
	var amp = 0.1*velocity/127.0;
	var env1, env2, son, pitch;
	pitch = freq.cpsmidi;
	pitch = Lag.kr(pitch, 0.12 * (1-Trig.kr(gate, 0.001)) * gate);
	env1 = EnvGen.ar(Env.new([0, 1.0, 0, 0], [0.001, 2.0, 0.04], [0, -4, -4], 2), gate, amp);
	env2 = EnvGen.ar(Env.adsr(0.001, 0.8, 0, 0.8, 70, -4), gate);
	son = LFPulse.ar(pitch.midicps, 0.0, 0.51, 2, -1);

	son = RLPF.ar(son, (pitch + env2).midicps, 0.3);
	// son = son * env1;

	// Out.ar(out, Pan2.ar(son, pan));
	OffsetOut.ar(out, DirtPan.ar(son/2, ~dirt.numChannels, pan, env1));
},
metadata: (
	credit: "from 08091500Acid309 by_otophilia",
	tags: [\pitched, \acid, \phat, \subtractive]
	)
).add
);
