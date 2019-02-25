// {Pan2.ar(Gendy3.ar(knum:MouseX.kr(2,13),mul:0.2), 0.0)}.play
/*(
{Pan2.ar(
Mix.new(Gendy3.ar(3,5,1.0,1.0,(Array.fill(5,{LFNoise0.kr(1.3.rand,1,2)})*MouseX.kr(100,378,'exponential')),MouseX.kr(0.01,0.05),MouseY.kr(0.001,0.016),5,mul:0.1)))}.play
)*/

/*{Pan2.ar(Gendy3.ar(1,6,LFPulse.kr(LFNoise0.kr(19.0,0.5,0.6), 0, 0.4, 0.5), Gendy1.kr(durscale:0.01,ampscale:0.01), MouseX.kr(10,100),mul:0.2), 0.0)}.play*/


(
SynthDef(\scream01, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	fb=1,indexModAttack=1.0,indexMod=1.0,
	attack=0.002,decay=0.49,sus=0.25,release=0.15,sustain
	|
	var smoothFreq, vibFreq, osc, mod, panner, env, noise;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,1,0.02);

	// mod = EnvGen.ar(Env.adsr(0.002, 0.5, 0.25, 0.25, 1, -4), gate, doneAction:0);
	mod = EnvGen.ar(Env.perc, gate, timeScale:sustain);
	osc = (FBSineC.ar(vibFreq*2, LFNoise2.kr(Lag.kr(1*indexModAttack),Lag.kr(16*indexMod),17), fb, 1.005, 0.7)*10 ).tanh;
	env = EnvGen.ar(Env.linen(attack,decay*0.8,release*0.8,sus,'lin'), timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar(osc.tanh, ~dirt.numChannels, pan, env));
}).add
);
//
// // default initial params
// { FBSineC.ar(SampleRate.ir/4) * 0.2 }.play(s);
//
// // increase feedback
// { FBSineC.ar(SampleRate.ir, 1, Line.kr(0.01, 4, 10), 1, 0.1) * 0.2 }.play(s);
//
// // increase phase multiplier
// {( FBSineC.ar(SampleRate.ir, 1, 0, XLine.kr(1, 2, 10), 0.1) * 0.2 ).tanh}.play(s);
//
// // modulate frequency and index multiplier
// { (FBSineC.ar(LFNoise2.kr(1, 1e4, 1e4), LFNoise2.kr(1,16,17), 1, 1.005, 0.7) * 0.2).tanh }.play(s);
//
// // randomly modulate params
// (
// { FBSineC.ar(
// 	LFNoise2.kr(1, 1e4, 1e4),
// 	LFNoise2.kr(1, 32, 33),
// 	LFNoise2.kr(1, 0.5),
// 	LFNoise2.kr(1, 0.05, 1.05),
// 	LFNoise2.kr(1, 0.3, 0.3)
// ) * 0.2 }.play(s);
// )
