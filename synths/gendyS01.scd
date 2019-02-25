
(
SynthDef(\gendyS01, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	points=12,brightness=2.0,
	attack=0.02,decay=0.2,sus=1,release=0.5,sustain
	|
	var smoothFreq, vibFreq, osc, mod, panner, env, noise,finalSignal;
	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,1,0.02);
	mod = EnvGen.ar(Env.adsr(0.002, 0.5, 0.25, 0.25, 1, -4), gate, doneAction:0);
	osc = Gendy3.ar(ampdist:1, durdist: 3, adparam: 1.0, ddparam: 1.0, freq: vibFreq, ampscale: 0.1, durscale: 0.0, initCPs: 12, knum:points, mul: 4.0, add: 0.0).tanh;

	finalSignal = BMoog.ar(in:osc/2,freq:smoothFreq*brightness,q:0.12,mode: 0,saturation: 0.95);
	env = EnvGen.ar(Env.linen(attack, decay, release, sus, 'lin'), gate, timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar(finalSignal/2, ~dirt.numChannels, pan, env));
}).add
);

// Synth(\gendyS01);

/*//stochastic waveform distortion- also play me at the same time as the previous example...
{Pan2.ar(Gendy3.ar(1,2,0.3,-0.7,MouseX.kr(55,110,'exponential'),0.03,0.1))}.play


(
{Pan2.ar(
Normalizer.ar(
RLPF.ar(
RLPF.ar(Mix.new(Gendy3.ar(freq:[230, 419, 546, 789])),
MouseX.kr(10,10000,'exponential'),0.05),
MouseY.kr(10,10000,'exponential'),0.05)
,0.9)
,Lag.kr(LFNoise0.kr(1),0.5))}.play
)*/


/*//concrete pH?
(
{Pan2.ar(
Mix.new(Gendy3.ar(freq:([1,1.2,1.3,1.76,2.3]*MouseX.kr(3,17,'exponential')),mul:0.2)))}.play
)

//glitch low, mountain high
(
{Pan2.ar(
Mix.new(Gendy3.ar(3,5,1.0,1.0,(Array.fill(5,{LFNoise0.kr(1.3.rand,1,2)})*MouseX.kr(100,378,'exponential')),MouseX.kr(0.01,0.05),MouseY.kr(0.001,0.016),5,mul:0.1)))}.play
)

//play me
{Pan2.ar(RLPF.ar(Gendy3.ar(1,3,freq:MouseX.kr(100,1000), durscale:0.0, ampscale:MouseY.kr(0.0,0.1), initCPs:7, knum: MouseY.kr(7,2)), 500,0.3, 0.2), 0.0)}.play



//used as an LFO
(
{Pan2.ar(SinOsc.ar(Gendy3.kr(2,5,SinOsc.kr(0.1,0,0.49,0.51),SinOsc.kr(0.13,0,0.49,0.51), 0.34, SinOsc.kr(0.17,0,0.49,0.51), SinOsc.kr(0.19,0,0.49,0.51),10,10,mul:50, add:350), 0, 0.3), 0.0)}.play
)

//buzzpipes
{Pan2.ar(Mix.new(Gendy3.ar(0, 0, SinOsc.kr(0.1, 0, 0.1, 0.9),1.0, [100,205,410], 0.011,0.005, 12, 12, 0.12)), 0.0)}.play


//modulate distributions
//change of pitch as distributions change the duration structure and spectrum
{Pan2.ar(Gendy3.ar(MouseX.kr(0,7),MouseY.kr(0,7),mul:0.2), 0.0)}.play


//modulate num of CPs
{Pan2.ar(Gendy3.ar(knum:MouseX.kr(2,13),mul:0.2), 0.0)}.play


//Gendy1 into Gendy2 into Gendy3...with cartoon side effects
(
{Pan2.ar(Gendy3.ar(1,2,freq:Gendy2.ar(maxfreq:Gendy1.kr(5,4,0.3, 0.7, 0.1, MouseY.kr(0.1,10), 1.0, 1.0, 5,5, 25,26),minfreq:24, knum:MouseX.kr(1,13),mul:150, add:200), durscale:0.01, ampscale:0.01, mul:0.1), 0.0)}.play
)

//use SINUS to track any oscillator and take CP positions from it, use adparam and ddparam as the inputs to sample
{Pan2.ar(Gendy3.ar(6,6,LFPulse.kr(LFNoise0.kr(19.0,0.5,0.6), 0, 0.4, 0.5), Gendy1.kr(durscale:0.01,ampscale:0.01), MouseX.kr(10,100),mul:0.2), 0.0)}.play



//wolf tones
(
{
Mix.fill(10,{
var freq;

freq= exprand(130,1160.3);
Pan2.ar(SinOsc.ar(Gendy3.ar(6.rand,6.rand,SinOsc.kr(0.1,0,0.49,0.51),SinOsc.kr(0.13,0,0.49,0.51),freq, SinOsc.kr(0.17,0,0.0049,0.0051), SinOsc.kr(0.19,0,0.0049,0.0051), 12, 12, 200, 400), 0, 0.1), 1.0.rand2)
});
}.play
)*/

//CAREFUL! mouse to far right causes explosion of sound-
//notice how high frequency and num of CPs affects CPU cost
// (
// {Pan2.ar(
// 	CombN.ar(
// 		Resonz.ar(
// 			Gendy3.ar(2,3,freq:MouseX.kr(10,700), initCPs:100),
// 		MouseY.kr(50,1000), 0.1)
// 		,0.1,0.1,5, 0.16
// 	)
// , 0.0)}.play
// )
//
//
// //storm
// (
// {
// 	var n;
// 	n=15;
//
// 	0.5*Mix.fill(n,{
// 		var freq, numcps;
//
// 		freq= rrand(130,160.3);
// 		numcps= rrand(2,20);
// 		Pan2.ar(Gendy3.ar(6.rand,6.rand,10.0.rand,10.0.rand,freq*exprand(1.0,2.0), 10.0.rand, 10.0.rand, numcps, SinOsc.kr(exprand(0.02,0.2), 0, numcps/2, numcps/2), 0.5/(n.sqrt)), 1.0.rand2)
// 	});
// }.play
// )
//
//
//
// //another glitchy moment
// (
// {
// 	var n;
// 	n=10;
//
// 	Resonz.ar(
// 		Mix.fill(n,{
// 			var freq, numcps;
//
// 			freq= rrand(50,560.3);
// 			numcps= rrand(2,20);
// 			Pan2.ar(Gendy3.ar(6.rand,6.rand,1.0.rand,1.0.rand,freq, 1.0.rand, 1.0.rand, numcps, SinOsc.kr(exprand(0.02,0.2), 0, numcps/2, numcps/2), 0.5/(n.sqrt)), 1.0.rand2)
// 		})
// 	,MouseX.kr(100,2000), MouseY.kr(0.01,1.0), 0.3)
// 	;
// }.play
// )