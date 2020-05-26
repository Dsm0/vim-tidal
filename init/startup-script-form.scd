var on, off, cc;
var osc;


// configure the sound server: here you could add hardware specific options
// see http://doc.sccode.org/Classes/ServerOptions.html
s.options.numBuffers = 1024 * 256; // increase this if you need to load more samples
s.options.memSize = 8192 * 32; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 32; // increase this if you are getting drop outs and the message "too many nodes"
s.options.numOutputBusChannels = 8; // set this to your hardware output channel size, if necessary
s.options.numInputBusChannels = 2; // set this to your hardware output channel size, if necessary
// boot the server and start SuperDirt
s.waitForBoot {
	~dirt = SuperDirt(2, s); // twelve output channels, increase if you want to pan across more channels
	~dirt.loadSoundFiles;   // load samples (path containing a wildcard can be passed in)
	// for example: ~dirt.loadSoundFiles("/Users/myUserName/Dirt/samples/*");
	// s.sync; // optionally: wait for samples to be read
    //
    //
	//~dirt.start(57120,outBusses: [0,1,2,3,4,5,6]);   // start listening on port 57120, create two busses each sending audio to channel 0
	~dirt.start(57120,outBusses: [0,0,2,2,4,4,6,6,8,8]);   // start listening on port 57120, create two busses each sending audio to channel 0

//	SynthDef(\sy1, {|out=0, freq=440| Out.ar(out, DC.ar(0)) }).add;
//	SynthDef(\sy2, {|out=0, freq=440| Out.ar(out, DC.ar(0)) }).add;
//	SynthDef(\sy3, {|out=0, freq=440| Out.ar(out, DC.ar(0)) }).add;
//	SynthDef(\sy4, {|out=0, freq=440| Out.ar(out, DC.ar(0)) }).add;
//	SynthDef(\sy5, {|out=0, freq=440| Out.ar(out, DC.ar(0)) }).add;
//	SynthDef(\sy6, {|out=0, freq=440| Out.ar(out, DC.ar(0)) }).add;
//	SynthDef(\sy7, {|out=0, freq=440| Out.ar(out, DC.ar(0)) }).add;
//	SynthDef(\sy8, {|out=0, freq=440| Out.ar(out, DC.ar(0)) }).add;
//	SynthDef(\sy9, {|out=0, freq=440| Out.ar(out, DC.ar(0)) }).add;
//	~dirt.soundLibrary.addSynth(\sy1);
//	~dirt.soundLibrary.addSynth(\sy2);
//	~dirt.soundLibrary.addSynth(\sy3);
//	~dirt.soundLibrary.addSynth(\sy4);
//	~dirt.soundLibrary.addSynth(\sy5);
//	~dirt.soundLibrary.addSynth(\sy6);
//	~dirt.soundLibrary.addSynth(\sy7);
//	~dirt.soundLibrary.addSynth(\sy8);
//	~dirt.soundLibrary.addSynth(\sy9);
	s.volume = (0);
//	~dirt.loadSoundFiles("/run/media/will/5EB068D0139EDA18/rap/*");
//	~dirt.loadSoundFiles("/run/media/will/5EB068D0139EDA18/songSamp/*");
//	~dirt.loadSoundFiles("/run/media/will/5EB068D0139EDA18/customSamples/*");
//	~dirt.loadSoundFiles("/M/Documents/unsorted_wavs");
};

//To accept MIDI in to Tidal we need to run a bit more code - this lets SuperCollider convert MIDI inputs from your controller into OSC messages

// Evaluate the block below to start the mapping MIDI -> OSC.

(
osc = NetAddr.new("127.0.0.1", 6010);

MIDIClient.init;
MIDIIn.connectAll;

on = MIDIFunc.noteOn({ |val, num, chan, src|

osc.sendMsg("/ctrl", "n", num); // if I remember correctly this was the bit I changed to get notes in!

});

off = MIDIFunc.noteOff({ |val, num, chan, src|
osc.sendMsg("/ctrl", "n", 0);
});

cc = MIDIFunc.cc({ |val, num, chan, src|
osc.sendMsg("/ctrl", num.asString, val/127);

});

if (~stopMidiToOsc != nil, {
~stopMidiToOsc.value;
});

~stopMidiToOsc = {
on.free;
off.free;
cc.free;
};
);


s.latency = 0.3; //increase this if you get "late" messages
//dirt.startSendRMS
