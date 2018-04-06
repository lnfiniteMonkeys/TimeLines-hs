s.boot;
s.quit

OSCdef.freeAll
{PinkNoise.ar(0.1)}.play;





~timeSynth.set(\r, 2000)
~timeSynth.set(\cycleDur, 2)


s.plotTree



(
s.freeAll;

~t = Bus.audio(s, 1);
~reverbBus = Bus.audio(s, 2);
~out = Bus.audio(s, 2);

SynthDef(\phasor, {
	arg cycleDur = 1, r = 1;
	Out.ar(~t, LFSaw.ar(cycleDur.reciprocal).range(0, r));
}).add;

SynthDef(\reverb, {
	arg in, predelay=0.1, revtime=1.8,
	lpf=4500, mix=0.15, amp=1, out=0;

	var dry, wet, temp, sig;
	dry = In.ar(in,2);
	temp = In.ar(in,2);
	wet = 0;
	temp = DelayN.ar(temp, 0,2, predelay);
	16.do{
		temp = AllpassN.ar(temp, 0.05, {Rand(0.001,0.05)}!2, revtime);
		temp = LPF.ar(temp, lpf);
		wet = wet + temp;
	};
	sig = XFade2.ar(dry, wet, mix*2-1, amp);
	Out.ar(out, sig);
}).add;

SynthDef(\fm, {
	arg amp, freq, ratio, index, pan;

	var freq_ = BufRd.kr(1, freq, In.ar(~t));
	var ratio_ = BufRd.kr(1, ratio, In.ar(~t));
	var index_ = BufRd.kr(1, index, In.ar(~t));
	var amp_ = BufRd.kr(1, amp, In.kr(~t));
	var pan_ = BufRd.kr(1, pan, In.ar(~t));

	var mod = SinOsc.ar(freq_*ratio_)*index_;
	var sig = SinOsc.ar(freq_ + mod) * amp_;

	Out.ar(0, Pan2.ar(sig, pan_));
}).add;

SynthDef(\fm2, {
	arg amp, freq, ratio, index, pan;

	var freq_ = PlayBuf.kr(1, freq, loop: 1);
	var ratio_ = PlayBuf.kr(1, ratio, loop: 1);
	var index_ = PlayBuf.kr(1, index, loop: 1);
	var amp_ = PlayBuf.kr(1, amp, loop: 1);
	var pan_ = PlayBuf.kr(1, pan, loop: 1);

	var mod = SinOsc.ar(freq_*ratio_)*index_;
	var sig = SinOsc.ar(freq_ + mod) * amp_;

	Out.ar(0, Pan2.ar(sig, pan_));
}).add;

SynthDef(\sine, {
	arg amp, freq, pan;

	var amp_ = BufRd.kr(1, amp, In.ar(~t));
	var freq_ = BufRd.kr(1, freq, In.ar(~t));
	var pan_ = BufRd.kr(1, pan, In.ar(~t));


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

	if(synth.isNil, {~synthDict.add(synthName -> Synth(synthDef, target: ~mainGrp))}, {});
	~buffDict.add(buffName.asSymbol -> Buffer.read(s, fileName, action: {|b| ~synthDict[synthName].set(synthParam, b)}));
};


~reset = {
	s.freeAll;
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



~buffDict[\bob_fm2_amp].plot


~b = Buffer.read(s,"/home/w1n5t0n/code/timelines/buffers/bob_fm_amp.w64" );



