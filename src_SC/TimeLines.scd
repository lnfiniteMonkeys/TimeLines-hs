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
	var fileName = "/home/carl/code/timelines/" ++ param ++ ".w64";
	var key = param.asSymbol;
	~d.add(key -> Buffer.read(s, fileName, action: {|b| x.set(key, b)}));
};


OSCdef(\TL_load, {
	|msg, time, addr, recvPort|
	~func.(msg[1]);
}, "/TimeLines/load",).add;

OSCdef(\TL_play, {
	|msg, time, addr, recvPort|
	~func.(msg[1]);
}, "/TimeLines/play",).add;

)

NetAddr.localAddr

OSCdef.trace(true);
OSCdef.trace(false);

~d[\freq].plot;

~n = Buffer.read(s, "/home/carl/code/timelines/amp.w64");
~n.plot


(
x = Synth(\fm);
)

s.meter

(
~amp.plot;
~freq.plot;
~ratio.plot;
~index.plot;
~envTest.plot;
)

