/*
Mitchell Sigman (2011) Steal this Sound. Milwaukee, WI: Hal Leonard Books
pp. 10-11
Adapted for SuperCollider and elaborated by Nick Collins
http://www.sussex.ac.uk/Users/nc81/index.html
under GNU GPL 3 as per SuperCollider license
Minor modifications by Bruno Ruviaro, June 2015.
Edited for use in tidalcycles by David Minnix 2018.
*/

SynthDef("tribell",{
	| out = 0, attack= 0.01, decay = 0.1, sustain = 1, release = 0.5, pan = 0.0, freq = 440, velocity = 100, gate = 1,  lforate = 10, lfowidth = 0.0, cutoff = 100, rq = 0.5|

	var osc1, osc2, vibrato, filter, env, amplitude, sound;
	amplitude = velocity/127.0;
	vibrato = SinOsc.ar(lforate, Rand(0, 2.0));
	osc1 = Saw.ar(freq * (1.0 + (lfowidth * vibrato)), 0.75);
	osc2 = Mix(LFTri.ar((freq.cpsmidi + [11.9, 12.1]).midicps));
	filter = RHPF.ar((osc1 + (osc2 * 0.5)) * 0.5, cutoff, rq);
	env = EnvGen.ar(
		envelope: Env.adsr(attack, decay, sustain, release, amplitude),
		gate: gate,
		doneAction: 2);
	// Out.ar(out, Pan2.ar(filter * env, pan));
	sound = filter;
	OffsetOut.ar(out, DirtPan.ar(sound/2, ~dirt.numChannels, pan, env));
}).add;
