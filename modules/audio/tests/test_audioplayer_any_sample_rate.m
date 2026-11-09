%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
% A player runs at its own sample rate whatever the device supports natively:
% a device without system mixer is fed through a software rate conversion, and
% the sample position, the total and the end of play stay in source samples.
%=============================================================================
skip_testsuite(iswasm(), ...
  'WebAssembly audio playback is queued through browser actions; this test targets PortAudio streams.');
skip_testsuite(ismac() && strcmp(getenv('GITHUB_ACTIONS'), 'true'), ...
  'The macOS GitHub runner audio device does not play in real time.');
%=============================================================================
for fs = [8192, 11025, 22050]
  y = 0.02 * sin(2 * pi * 440 * (0:round(0.4 * fs) - 1)' / fs);
  p = audioplayer(y, fs);
  asserts.isequal(p.SampleRate, fs);
  asserts.isequal(p.TotalSamples, numel(y));
  play(p);
  t0 = tic();
  while strcmp(p.Running, 'on') && toc(t0) < 3
    pause(0.01);
  end
  elapsed = toc(t0);
  asserts.isequal(p.Running, 'off');
  asserts.isequal(p.CurrentSample, p.TotalSamples);
  % about 0.4 s of audio: neither skipped nor played at the device rate
  asserts.istrue(elapsed > 0.2 && elapsed < 2.5, sprintf('played %g s at %d Hz', elapsed, fs));
  delete(p);
end
%=============================================================================
% integer samples and two channels
fs = 8192;
y16 = int16(round(1000 * sin(2 * pi * 300 * (0:round(0.2 * fs) - 1)' / fs)));
p = audioplayer([y16, y16], fs);
asserts.isequal(p.NumChannels, 2);
playblocking(p);
asserts.isequal(p.Running, 'off');
delete(p);
%=============================================================================
