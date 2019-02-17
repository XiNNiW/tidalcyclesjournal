/*
This is an example startup file. You can load it from your startup file
(to be found in Platform.userAppSupportDir +/+ "startup.scd")
*/

/*
This is an example startup file. You can load it from your startup file
(to be found in Platform.userAppSupportDir +/+ "startup.scd")
*/

// configure the sound server: here you could add hardware specific options
// see http://doc.sccode.org/Classes/ServerOptions.html
s.options.numBuffers = 1024 * 256; // increase this if you need to load more samples
s.options.memSize = 8192 * 16; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 32; // increase this if you are getting drop outs and the message "too many nodes"
s.options.numOutputBusChannels = 10; // set this to your hardware output channel size, if necessary
s.options.numInputBusChannels = 2; // set this to your hardware output channel size, if necessary
// s.options.maxLogins = 8;
// boot the server and start SuperDirt
s.waitForBoot {
	~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
	~dirt.loadSoundFiles;   // load samples (path containing a wildcard can be passed in)
	~dirt.loadSoundFiles("~/Music/tidal-cycles/samples/*");
	~dirt.loadSynthDefs("~/Music/tidal-cycles/synths/");
	// for example: ~dirt.loadSoundFiles("/Users/myUserName/Dirt/samples/*");
	// ~dirt.startSendRMS;
	s.sync; // wait for samples to be read
	~dirt.start(57120, [0, 2, 4, 6, 8, 10, 12,14]);   // start listening on port 57120, create two busses each sending audio to channel 0

	// optional, needed for the sclang tests only:
	// (
	//
	// 	~d1 = ~dirt.orbits[0]; // one orbit
	// 	~d2 = ~dirt.orbits[1];
	//
	// );

	s.latency = 2; // increase this if you get "late" messages



	// version 0.2

	// first, initialize your midi device.
	MIDIClient.init;

	MIDIClient.list; // to check, which are there, you can query them

	// create a midi out connection
	// ~midiOut = MIDIOut.newByName("FastLane USB", "Port A"); // substitute your own device here
	~midiOutput = MIDIOut.newByName("Virtual Raw MIDI 0-0","VirMIDI 0-0");
	~dirt.soundLibrary.addMIDI(\midi, ~midiOutput);
	~midiOutput.latency = 0.1;

	 // you may want to adjust the latency here
	// ~midiOut.latency = 0.1;

	// define one instrument called "midi" (you may call it what you like)
	// it uses the built in event type "midi" (this is predefined).


};
