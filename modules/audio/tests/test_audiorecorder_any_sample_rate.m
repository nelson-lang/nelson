%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--AUDIO INPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% A recorder records at its own sample rate whatever the device supports
% natively: the recorded frame count follows the requested rate.
%=============================================================================
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend does not provide microphone recorder objects yet.');
%=============================================================================
for fs = [8000, 11025]
  r = audiorecorder(fs, 16, 1);
  asserts.isequal(r.SampleRate, fs);
  recordblocking(r, 1);
  y = getaudiodata(r);
  asserts.isequal(size(y, 2), 1);
  % one second at the requested rate, give or take a device buffer
  asserts.istrue(abs(numel(y) - fs) <= 0.1 * fs, sprintf('%d frames at %d Hz', numel(y), fs));
  asserts.isequal(r.TotalSamples, numel(y));
  delete(r);
end
%=============================================================================
