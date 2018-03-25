s.boot;

OSCdef.freeAll

(
~t = Bus.control(s, 1);

SynthDef(\fm, {
	arg amp, freq, ratio, index;

	var freq_ = PlayBuf.kr(1, freq, loop: 1);
	var ratio_ = PlayBuf.kr(1, ratio, loop: 1);
	var index_ = PlayBuf.kr(1, index, loop: 1);
	var mod_ = SinOsc.ar(freq_*ratio_)*index_;
	var amp_ = PlayBuf.kr(1, amp, loop: 1);
	var sig = SinOsc.ar(freq_ + mod_) * amp_;
	Out.ar(0, sig!2);
}).add;

~d = Dictionary.new();

~func = { |param|
	//build path for file to load as buffer
	var fileName = "/home/carl/code/timelines/" ++ param.asString.drop(3)++".w64";
	//extract key from filepath and store at dict
	var n = param.asString.split()[2];
	var key = n.asSymbol;
	var synthArg = n.split($_)[1].postln;
	~d.add(key -> Buffer.read(s, fileName, action: {|b| x.set(synthArg, b)}));
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
x = Synth(\fm);
)

param.asString.split()[2];

~d[\perc_amp].plot;
~d[\perc_freq].plot;
~d[\perc_ratio].plot;
~d[\perc_index].plot;
