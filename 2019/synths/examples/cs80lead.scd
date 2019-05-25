// based on https://github.com/supercollider-quarks/SynthDefPool/blob/master by danstowell
// adapted to work in tidal cycles by David Minnix
(
SynthDef(\cs80lead, {
	| freq=880, velocity = 100, attack=0.75, decay=0.5, sustain=0.8, release=1.0, fattack=0.75, fdecay=0.5, fsustain=0.8, frelease=1.0,
		cutoff=200, pan=0, dtune=0.002, vibrate=4, vibdepth=0.015, gate=1.0, ratio=1,out=0,cbus=1|
	var env,fenv,vib,ffreq,sig;
	var amp = 0.5*velocity/127.0;
	cutoff=In.kr(cbus);
	env=EnvGen.kr(Env.adsr(attack,decay,sustain,release),gate,levelScale:1,doneAction:2);
	fenv=EnvGen.kr(Env.adsr(fattack,fdecay,fsustain,frelease,curve:2),gate,levelScale:1,doneAction:2);
	vib=SinOsc.kr(vibrate).range(-1*vibdepth,vibdepth)+1;
	freq=Line.kr(freq,freq*ratio,5);
	freq=freq*vib;
	sig=Mix.ar(Saw.ar([freq,freq*(1+dtune)],mul:env*amp));
	// keep this below nyquist!!
	ffreq=max(fenv*freq*12,cutoff)+100;
	sig=LPF.ar(sig,ffreq);
	// Out.ar(out, Pan2.ar(sig,pan) );
	OffsetOut.ar(out, DirtPan.ar(sig/2, ~dirt.numChannels, pan, env));
}).add
// },
// metadata: (
// 	credit: "Vangelis/Blade Runner lead sound, based on tutorial by meastempo @ http://www.youtube.com/watch?v=Fne0oIEv-WI",
// 	tags: [\lead, \modulation, \analog, \cs80, \vangelis, \bladerunner]
// 	)
// )
);
