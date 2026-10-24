%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--AUDIO OUTPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
skip_testsuite(iswasm(), ...
  'WebAssembly audio playback is queued through browser actions; this desktop lifecycle timing test targets PortAudio stream state.');
%=============================================================================
% Sequences of non-blocking players: each one must play in real time,
% whatever happened to the previous one (natural end, stop, delete).
%=============================================================================
function waitStarted(p)
  t0 = tic();
  while p.CurrentSample == 0 && toc(t0) < 1
    pause(0.01);
  end
end
%=============================================================================
function r = playbackRate(p, fs)
  waitStarted(p);
  s1 = p.CurrentSample;
  t = tic();
  pause(0.25);
  s2 = p.CurrentSample;
  r = (s2 - s1) / fs / toc(t);
end
%=============================================================================
function checkRealTime(p, fs)
  asserts.isequal(p.Running, 'on');
  r = playbackRate(p, fs);
  asserts.istrue(r > 0.5 && r < 1.5, sprintf('playback rate %g, real time expected', r));
  asserts.isequal(p.Running, 'on');
end
%=============================================================================
function waitFinished(p)
  t0 = tic();
  while strcmp(p.Running, 'on') && toc(t0) < 2
    pause(0.01);
  end
end
%=============================================================================
fs = 44100;
y = 0.02 * sin(2 * pi * 440 * (0:fs - 1)' / fs);
short = y(1:round(0.05 * fs));
%=============================================================================
% Probe the output device once: this suite asserts that playback advances in
% real time, which an emulated or non-draining device (e.g. a headless VM sound
% card) cannot satisfy. Skip when the real-time capability is absent; a working
% device plays at ~1x and runs the full suite normally.
probe = audioplayer(y, fs);
play(probe);
probeRate = playbackRate(probe, fs);
stop(probe);
delete(probe);
skip_testsuite(~(probeRate > 0.5 && probeRate < 1.5), ...
  'audio output device does not play in real time');
%=============================================================================
% first player of the sequence
p = audioplayer(y, fs);
play(p);
checkRealTime(p, fs);
stop(p);
asserts.isequal(p.Running, 'off');
asserts.isequal(p.CurrentSample, 0);
delete(p);
%=============================================================================
% after a stopped and deleted player
p = audioplayer(y, fs);
play(p);
checkRealTime(p, fs);
stop(p);
delete(p);
%=============================================================================
% after a natural end
q = audioplayer(short, fs);
play(q);
waitFinished(q);
asserts.isequal(q.Running, 'off');
if iswasm()
  asserts.istrue(abs(q.CurrentSample - q.TotalSamples) <= 1);
else
  asserts.isequal(q.CurrentSample, q.TotalSamples);
end
p = audioplayer(y, fs);
play(p);
checkRealTime(p, fs);
stop(p);
delete(p);
%=============================================================================
% after deleting a naturally finished player
delete(q);
p = audioplayer(y, fs);
play(p);
checkRealTime(p, fs);
stop(p);
delete(p);
%=============================================================================
% after playblocking
q = audioplayer(short, fs);
playblocking(q);
asserts.isequal(q.Running, 'off');
delete(q);
p = audioplayer(y, fs);
play(p);
checkRealTime(p, fs);
stop(p);
delete(p);
%=============================================================================
% same player replayed after its natural end
q = audioplayer(short, fs);
play(q);
waitFinished(q);
play(q);
asserts.isequal(q.Running, 'on');
waitFinished(q);
if iswasm()
  asserts.istrue(abs(q.CurrentSample - q.TotalSamples) <= 1);
else
  asserts.isequal(q.CurrentSample, q.TotalSamples);
end
delete(q);
%=============================================================================
% deleting a playing player
p = audioplayer(y, fs);
play(p);
waitStarted(p);
delete(p);
p = audioplayer(y, fs);
play(p);
checkRealTime(p, fs);
delete(p);
%=============================================================================
% 30 short players in a row, each one played to its end, no leaked stream
players = cell(1, 30);
for k = 1:30
  players{k} = audioplayer(short, fs);
  play(players{k});
  waitFinished(players{k});
  asserts.isequal(players{k}.Running, 'off');
  if iswasm()
    asserts.istrue(abs(players{k}.CurrentSample - players{k}.TotalSamples) <= 1);
  else
    asserts.isequal(players{k}.CurrentSample, players{k}.TotalSamples);
  end
end
p = audioplayer(y, fs);
play(p);
checkRealTime(p, fs);
stop(p);
delete(p);
for k = 1:30
  delete(players{k});
end
%=============================================================================
