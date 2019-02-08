(

 SynthDef(\blip, {|freq=220, amp=1, gate=1, sustain, pan=0, out=0|
 	var lag, blip, panner, env;
 	lag = Lag.kr(freq);
 	blip = Blip.ar(lag, 10, 0.2);
 	env = EnvGen.kr(Env.asr, gate, timeScale:sustain, doneAction:2);
 	OffsetOut.ar(out, DirtPan.ar(blip/2, ~dirt.numChannels, pan, env));

 }).add

);