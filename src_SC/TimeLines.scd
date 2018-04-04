s.boot;

OSCdef.freeAll

{PinkNoise.ar(0.1)}.play;


s.boot;


~synthDict


(
~t = Bus.control(s, 1);

SynthDef(\fm, {
	arg amp, freq, ratio, index, pan;

	var freq_ = PlayBuf.kr(1, freq, loop: 1);
	var ratio_ = PlayBuf.kr(1, ratio, loop: 1);
	var index_ = PlayBuf.kr(1, index, loop: 1);
	var mod_ = SinOsc.ar(freq_*ratio_)*index_;
	var amp_ = PlayBuf.kr(1, amp, loop: 1);
	var sig = SinOsc.ar(freq_ + mod_) * amp_;

	var pan_ = PlayBuf.kr(1, pan, loop: 1);
	Out.ar(0, Pan2.ar(sig, pan_));
}).add;

SynthDef(\sine, {
	arg amp, freq, pan;

	var amp_ = PlayBuf.kr(1, amp, loop: 1);
	var freq_ = PlayBuf.kr(1, freq, loop: 1);
	var pan_ = PlayBuf.kr(1, pan, loop: 1);
	var sig = SinOsc.ar(freq_) * amp_;
	Out.ar(0, Pan2.ar(sig, pan_));
}).add;

~buffDict = Dictionary.new();
~synthDict = Dictionary.new();

~func = { |param|
	//build path for file to load as buffer
	var buffName = param.asString.drop(3).split()[1];
	var fileName = "/home/carl/code/timelines/buffers/" ++ buffName ++ ".w64";

	var p = buffName.split($_).postln;
	var synthName = p[0].asSymbol;
	var synthDef = p[1];
	var synthParam = p[2].asSymbol;
	var synth = ~synthDict[synthName];

	if(synth.isNil, {~synthDict.add(synthName -> Synth(synthDef))}, {});

	~buffDict.add(buffName.asSymbol -> Buffer.read(s, fileName, action: {|b| ~synthDict[synthName].set(synthParam, b)}));
};



OSCdef(\TL_load, {
	|msg, time, addr, recvPort|
	msg.postln;
	~func.(msg[1]);
}, "/TimeLines/load",).add;

OSCdef(\TL_play, {
	|msg, time, addr, recvPort|
	~func.(msg[1]);
}, "/TimeLines/play",).add;

)



(
SynthDef(\test, {Out.ar(0, 0)}).add;
)
~dict[\bob].isNildefName;

(
//~dict = Dictionary.new();
~info = "bob_something_amp".split($_);
~key = ~info[0].asSymbol;
~synthDef = ~info[1];
~param = ~info[2];

if(~dict[~key].isNil, {~dict.add(~key -> Synth(\fm))});
if(~dict[~key].defName != ~synthDef, {~dict[~key].free; ~dict.add(~key -> Synth(~synthDef.asSymbol))}, {});
)


(
x = Synth(\fm);
)

param.asString.split()[2];

~d[\perc_amp].plot;
~d[\perc_freq].plot;
~d[\perc_ratio].plot;
~d[\perc_index].plot;
