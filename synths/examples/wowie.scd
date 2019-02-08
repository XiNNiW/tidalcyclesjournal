(
SynthDef(\superwow, {|out, sustain=1, pan, accelerate, freq, decay=0, slide=1, detune=1 |
	var env = EnvGen.ar(Env.pairs([[0,0],[0.07,1],[0.2,1-decay],[0.95,1-decay],[1,0]], -1), timeScale:sustain, doneAction:2);
	var basefreq = freq * Line.kr(1, 1+accelerate, sustain);
	var sound = PulseDPW.ar(basefreq, 0.5,  Line.ar(0,0.5,sustain*20/slide),0);
	var sound2 = PulseDPW.ar(basefreq*(detune/100+1), 0.5,  Line.ar(0,0.5,sustain*20/slide),0);
	var sound3 = PulseDPW.ar(basefreq*(-1*detune/100+1), 0.5,  Line.ar(0,0.5,sustain*20/slide),0);
	// var sound = VarSaw.ar(basefreq, 0, Line.ar(0,0.5,sustain*20/slide));
	// var sound2 = VarSaw.ar(basefreq*(detune/100+1), 0, Line.ar(0,0.5,sustain*20/slide));
	// var sound3 = VarSaw.ar(basefreq*(-1*detune/100+1), 0, Line.ar(0,0.5,sustain*20/slide));
	sound = sound - DelayN.ar(sound2,0.2, Line.ar(0,1,5*sustain/slide)/basefreq)
	  + DelayN.ar(sound3,0.2, Line.ar(0,1,20*sustain/slide)/basefreq);
	OffsetOut.ar(out, DirtPan.ar(sound/2, ~dirt.numChannels, pan, env));
}).add
);
