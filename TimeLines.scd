s.boot;

(
SynthDef(\fm, {
	arg ampBuf, freqBuf, modRBuf, modABuf, t;

	var freq = PlayBuf.kr(1, freqBuf, loop: 1);
	var ratio = PlayBuf.kr(1, modRBuf, loop: 1);
	var index = PlayBuf.kr(1, modABuf, loop: 1);
	var mod = SinOsc.ar(freq*ratio)*index;

	var amp = PlayBuf.kr(1, ampBuf, loop: 1);
	var sig = SinOsc.ar(freq + mod) * amp;
	Out.ar(0, sig!2);
}).add;


~reloadBuffers = {
	~amp = Buffer.read(s, "/home/carl/code/timelines/timelines/amp.w64");
	~freq = Buffer.read(s, "/home/carl/code/timelines/timelines/freq.w64");
	~ratio = Buffer.read(s, "/home/carl/code/timelines/timelines/ratio.w64");
	~index = Buffer.read(s, "/home/carl/code/timelines/timelines/index.w64");
	~envTest = Buffer.read(s, "/home/carl/code/timelines/timelines/envTest.w64");

	x.set(
		\ampBuf, ~amp,
		\freqBuf, ~freq,
		\modRBuf, ~ratio,
		\modABuf, ~index
	);
	"done".postln;
};


OSCdef(\TL, {
	|msg, time, addr, recvPort|
	~reloadBuffers.value();
	//3.postln;
	//~aFunc.value;
}, "/TimeLines",).add;



x = Synth(\fm, [
	\ampBuf, ~amp,
	\freqBuf, ~freq,
	\modRBuf, ~ratio,
	\modABuf, ~index]
);
)




(
~amp.plot;
~freq.plot;
~ratio.plot;
~index.plot;
~envTest.plot;
)

