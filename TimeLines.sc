TimeLineSC {
	*do {
		"test".postln;
	}
}e


~t = Bus.control(s, 1);
~t.scope.alwaysOnTop;


(
SynthDef(\time, {
	|t_gate = 0, dur = 1, start = 0, end = 1|

	//var env = Env([0, start, end, 0], [0, dur], 'lin', 3, 1).kr(0, t_gate);
	var env = Sweep.kr(t_gate, 20.5);
	Out.kr(~t, env);
}).add;
)

(
SynthDef(\lr, {
	|start = 0, end = 1, dur = 1, t_trig = 0|
    var playhead = (Sweep.kr(t_trig, 1/dur)).linlin(0, 1, start, end, \minmax);
    Out.kr(~t, playhead);
}).add;
)


(
SynthDef(\phasor, {
	|t_trig = 0, dur = 1, start = 0, end = 1|
	var playhead = Phasor.kr(t_trig, 1/dur, 0, 1, 0).linlin(0, 1, start, end, \minmax);
	Out.kr(~t, playhead);
}).add;
)


(
SynthDef(\env, {
	|trig = 0, dur = 1, start = 0, end = 1|
	var playhead = Env([0, 0, 1, 0], [0, dur, 0], 'lin', nil, 1).kr(trig, 0);
	Out.kr(~t, playhead);
}).add;
)


t = Synth(\env);
t.set(\trig, 1);

(
SynthDef(\sine, {
	|freq = 440, amp = 1000|
	var sig = SinOsc.ar(freq)*In.kr(~timeBus);
	Out.ar(0, sig!2);
}).add;
)


a = Synth(\lr);
a.set(\t_trig, 1);
a.set(\t_trig, 1, \dur, 3, \run, 1);
x = Synth(\sine);

s.plotTree