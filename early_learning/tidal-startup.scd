/*
This is an example startup file. You can load it from your startup file
(to be found in Platform.userAppSupportDir +/+ "startup.scd")
*/


(
// configure the sound server: here you could add hardware specific options
// see http://doc.sccode.org/Classes/ServerOptions.html
s.options.numBuffers = 1024 * 256; // increase this if you need to load more samples
s.options.memSize = 8192 * 16; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 32; // increase this if you are getting drop outs and the message "too many nodes"
//s.options.numOutputBusChannels = 2; // set this to your hardware output channel size, if necessary
s.options.numOutputBusChannels = 12;
s.options.numInputBusChannels = 2; // set this to your hardware output channel size, if necessary
// boot the server and start SuperDirt
s.waitForBoot {
	~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
	//~dirt.loadSoundFiles;   // load samples (path containing a wildcard can be passed in)
	~dirt.loadSoundFiles("~/Music/tidal-cycles/october-samples/*");
	// for example: ~dirt.loadSoundFiles("/Users/myUserName/Dirt/samples/*");
	// s.sync; // optionally: wait for samples to be read
//	~dirt.start(57120, [0, 0]);   // start listening on port 57120, create two busses each sending audio to channel 0
	~dirt.start(57120, [0, 2, 4, 6, 8, 10]);

/*	// optional, needed for the sclang tests only:
	(

		~d1 = ~dirt.orbits[0]; // one orbit
		~d2 = ~dirt.orbits[1];

	);*/


	MIDIClient.init;

	MIDIClient.list;

	~midiOutput = MIDIOut.newByName("Virtual Raw MIDI 0-0","VirMIDI 0-0");
	~midiOutput.latency = 0.1;

	~dirt.soundLibrary.addMIDI(\midi, ~midiOutput);

};

s.latency = 0.3; // increase this if you get "late" messages
);
