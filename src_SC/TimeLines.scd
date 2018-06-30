
(
s.freeAll;

~t = Bus.audio(s, 1);
~reverbBus = Bus.audio(s, 2);
~out = 0;



~func = { |path|
	var filePath = path.asString;
	var pathList = path.asString.split();
	var buffName = pathList[pathList.size - 1].split($.)[0];
	var info = buffName.split($_);

	var synthName = info[0].asSymbol;
	var synthDef = info[1];
	var synthParam = info[2].asSymbol;
	var synth = ~synthDict[synthName];

	if(synth.isNil, {~synthDict.add(synthName -> Synth(synthDef, target: ~mainGrp))}, {});
	~buffDict[buffName].free;
	~buffDict.add(buffName -> Buffer.read(s, filePath, action: {|b| ~synthDict[synthName].set(synthParam, b)}));
};


~reset = {
	s.freeAll;
	Buffer.freeAll;
	//s.freeAllBuffers;
	~buffDict = Dictionary.new();
	~synthDict = Dictionary.new();
	~timeGrp = Group.new;
	~mainGrp = Group.after(~timeGrp);
	~reverbGrp = Group.after(~mainGrp);
	~reverbSynth = Synth.new(
		\reverb,
		[
			\amp, 1,
			\predelay, 0.1,
			\revtime, 1.8,
			\lpf, 4500,
			\mix, 0.35,
			\in, ~reverbBus,
			\out, ~out,
		],
		~reverbGrp
	);
	~timeSynth = Synth(\phasor, [\cycleDur, 5, \r, 5*700], ~timeGrp);

	"reset done".postln;
};


OSCdef(\TL_load, {
	|msg, time, addr, recvPort|
	msg.postln;
	~func.(msg[1]);
}, "/TimeLines/load").add;

OSCdef(\TL_play, {
	|msg, time, addr, recvPort|
	~func.(msg[1]);
}, "/TimeLines/play").add;

OSCdef(\TL_window, {
	|msg, time, addr, recvPort|
	var dur = msg[1].asFloat;
	var sr = 700;
	var numFrames = sr*dur;

	~timeSynth.set(
		\cycleDur, dur,
		\r, numFrames);
}, "/TimeLines/window").add;

OSCdef(\TL_reset, {
	|msg, time, addr, recvPort|
	~reset.();
}, "/TimeLines/reset").add;

)

s.plotTree
(
~reverbSynth.set(
			\amp, 1,
			\predelay, 1.1,
			\revtime, 3.8,
			\lpf, 4500,
			\mix, 0.4,
			\in, ~reverbBus,
			\out, ~out
);)



~buffDict["s1_fm_amp"].plot


thisProcess.platform

Platform.case(
	\osx,       { "OSX".postln },
    \linux,     { "Linux".postln },
    \windows,   { "Windows".postln }
);


Platform.defaultTempDir

s.meter



a = { LPF.ar(Saw.ar(200), 600) * EnvGate.new }.play;
a.set(\fadeTime, 2);
a.release;