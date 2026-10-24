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
% <--AUDIO INPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend does not provide microphone recorder objects yet.');
%=============================================================================
devices = audiodevinfo('default');
sampleRate = devices.input.DefaultSampleRate;
myVoice = audiorecorder(sampleRate, 16, 1);
duration = 1;
tic(); recordblocking(myVoice, duration); elapsedTime = toc();
skip_testsuite(elapsedTime < 1, 'Audio input unavailable');
asserts.istrue(elapsedTime <= 30);
asserts.istrue(myVoice.TotalSamples >= sampleRate * duration);
%=============================================================================
playblocking(myVoice);
%=============================================================================
