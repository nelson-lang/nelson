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
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend uses the portable handle display, not the PortAudio desktop property display.');
%=============================================================================
signal = rand(2, 44100) - 0.5;
playObj = audioplayer(signal, 44100, 16);
%=============================================================================
R = evalc('disp(playObj)');
asserts.istrue(contains(R, ['audioplayer ', _('with properties:')]));
asserts.istrue(contains(R, 'SampleRate: 44100'));
asserts.istrue(contains(R, 'BitsPerSample: 16'));
asserts.istrue(contains(R, 'NumChannels: 2'));
asserts.istrue(contains(R, 'TotalSamples: 44100'));
asserts.istrue(contains(R, 'Running: off'));
asserts.isfalse(contains(R, 'Show all properties'));
asserts.istrue(contains(R, 'StartFcn'));
asserts.istrue(contains(R, 'TimerFcn'));
%=============================================================================
R = evalc('display(playObj)');
asserts.istrue(contains(R, 'playObj ='));
asserts.istrue(contains(R, ['audioplayer ', _('with properties:')]));
asserts.istrue(contains(R, 'SampleRate: 44100'));
asserts.isfalse(contains(R, 'Show all properties'));
asserts.istrue(contains(R, 'StartFcn'));
%=============================================================================
