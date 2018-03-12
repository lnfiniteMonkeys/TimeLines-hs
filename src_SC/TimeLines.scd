s.boot;

OSCdef.freeAll

(
~t = Bus.control(s, 1);

SynthDef(\fm, {
	arg amp, freq, ratio, index;

	var t = In.kr(~t, 1);

	var freq_ = BufRd.kr(1, freq, t, 1);
	var ratio_ = PlayBuf.kr(1, ratio, t, 1);
	var index_ = PlayBuf.kr(1, index, t, 1);
	var mod_ = SinOsc.ar(freq_*ratio_)*index_;
	var amp_ = PlayBuf.kr(1, amp, t, 1);
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

