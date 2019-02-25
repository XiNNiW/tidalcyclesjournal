
(

SynthDef(\granular01,{
	|
	out=0, gate = 1, amp = 1, sample=\sid,
	pan = 0,freq=440.0,harmonic=1.0,brightness=1.0,sustain|
  var env, freqdev, osc, winenv, windowbuf,sndbuf,grains,smoothFreq,vibFreq;
	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);

	winenv = Env([0, 1, 0], [0.5, 0.5], [8, -8]);
	windowbuf = Buffer.sendCollection(s, winenv.discretize, 1);
	if(~dirt.soundLibrary.buffers[sample].isNil,{
		sndbuf = ~dirt.soundLibrary.buffers[\sid]
	} , {
		sndbuf = ~dirt.soundLibrary.buffers[sample];
	});


	grains = GrainBuf.ar(1, Impulse.kr(LFNoise2.kr(3).range(5, 30)*Line.kr(1,0.2,1,1,0,0)), 0.1, sndbuf, LFNoise1.kr.range(0.5, 2),
            LFNoise2.kr(0.1).range(0, 1), 2, pan, windowbuf);
	osc = PMOsc.ar((vibFreq/2), harmonic*smoothFreq, brightness*100*(grains), 0, 0.1);
/*	osc = (FBSineC.ar(vibFreq, LFNoise2.ar(Lag.kr(1),Lag.kr(16),17), 1.0, 1.005, 0.7)*grains * 10).tanh;*/
	env = EnvGen.ar(Env.perc, gate, timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar((osc*(1+grains)).tanh/2, ~dirt.numChannels, pan, env));

}).add;
)


// Synth(\granular01,\cp);

