
(
var voice303;

/*var freqb=Bus.control(s,1);
var ampb=Bus.control(s,1);
var harmonicb=Bus.control(s,1);
var brightnessb=Bus.control(s,1);
var resonanceb = Bus.control(s,1);
var overdriveb=Bus.control(s,1);*/
var voiceOut = Bus.audio(s,1);
var voice303Controls = Bus.control(s,1);

SynthDef(\m303, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	attack = 0.2,sus=1,decay=0.8,release=0.3, sustain=0.3,
	harmonic=1.0, brightness=1,resonance = 0.5,overdrive=1
	|
	var env;

	Out.kr(voice303Controls,freq);
	//voice303Controls.set(freq);

	env = EnvGen.ar(
		Env.perc(attack,release),
		gate:gate,
		timeScale:sustain,
		doneAction:2
	);

	OffsetOut.ar(
		out,
		DirtPan.ar(
			In.ar(voiceOut,1)/2,
			~dirt.numChannels,
			pan,
			env)
	);


}).add;

SynthDef(\voice303,{
	arg out,controlBus;

	var osc = SinOsc.ar(In.kr(voice303Controls));

/*	var osc = BlitB3Square.ar(In.kr(freqb).lag);
	var filter = LPF18.ar(
		osc,
		In.kr(freqb)*In.kr(harmonicb)*In.kr(brightnessb),
		In.kr(resonanceb),
		In.kr(overdriveb.)
	);*/
	Out.ar(out,osc);

}).add;

voice303 = Synth(\voice303);
voice303.set("out",voiceOut);
voice303.set("controlBus",voice303Controls);

// freqb.set(420);


)

// Synth(\m303);

