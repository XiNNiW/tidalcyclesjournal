
(
SynthDef(\pm04, {
	|
	freq=220, amp=1, gate=1, pan=0,out=0,
	a=0.1, d=0.5, s=0.88, r=0.6, sustain,
	harmonic=1.5,brightness=0.5,
	stiffness=0.75,distance=0.5,spread=0
	|
	var smoothFreq, vibFreq, osc1, osc2, mod, springMod, inforce, panner, env,noise;
	// envelope times must add up to 1
	var envelopeScaleing = 1/(a+d+r);
	a = a*envelopeScaleing;
	d = d*envelopeScaleing;
	r = r*envelopeScaleing;

	smoothFreq = Lag.kr(freq);
	vibFreq = Vibrato.kr(smoothFreq,0.5,0.01);
	// modulation
	// mod = PMOsc.ar(harmonic*3*vibFreq, harmonic*vibFreq*5/3, Line.ar(0,200,1), 0, 0.1);
	inforce = K2A.ar(gate) > 0;
	stiffness = ControlSpec.new(minval: 0.1, maxval: 20.0, warp: 'lin').map(stiffness);//Line.kr(0.1, 20, 1);
	distance = ControlSpec.new(minval: 0.1, maxval: 0.0000001, warp:  'exp').map(distance);//Line.kr(0.00001, 0.1, 1);
	springMod = Spring.ar(inforce, stiffness, distance);


	noise = PinkNoise.ar(EnvGen.kr(Env.perc, doneAction: 0));
	osc1 = PMOsc.ar(vibFreq+spread, harmonic*0.5*vibFreq, (1+noise)*springMod*brightness, 0, 1);
	osc2 = PMOsc.ar(vibFreq-spread, harmonic*0.5*vibFreq, (1+noise)*springMod*brightness, 0, 1);
	env = EnvGen.kr(Env.linen(a,d,r,s,'lin'), timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar((osc1/4).tanh, ~dirt.numChannels, pan+spread, env));
	OffsetOut.ar(out, DirtPan.ar((osc2/4).tanh, ~dirt.numChannels, pan-spread, env));
}).add
)

// Synth(\pm04);

/*(
{
    var inforce, outforce, freq, k, d;
    inforce = K2A.ar(MouseButton.kr(0,1,0)) > 0;
    k = MouseY.kr(0.1, 20, 1);
    d = MouseX.kr(0.00001, 0.1, 1);
    outforce = Spring.ar(inforce, k, d);
    freq = outforce * 400 + 500; // modulate frequency with the force
    SinOsc.ar(freq, 0, 0.2)
}.play;
)*/