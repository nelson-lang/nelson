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
  'WebAssembly audio backend does not provide microphone recorder objects yet.');
%=============================================================================
recObj = audiorecorder();
%=============================================================================
R = evalc('disp(recObj)');
asserts.istrue(contains(R, ['audiorecorder ', _('with properties:')]));
asserts.istrue(contains(R, 'SampleRate:'));
asserts.istrue(contains(R, 'BitsPerSample:'));
asserts.istrue(contains(R, 'NumChannels:'));
asserts.istrue(contains(R, 'Running: off'));
asserts.isfalse(contains(R, 'Show all properties'));
asserts.isfalse(contains(R, 'NumberOfChannels'));
asserts.istrue(contains(R, 'StartFcn'));
%=============================================================================
R = evalc('display(recObj)');
asserts.istrue(contains(R, 'recObj ='));
asserts.istrue(contains(R, ['audiorecorder ', _('with properties:')]));
asserts.istrue(contains(R, 'NumChannels:'));
asserts.isfalse(contains(R, 'Show all properties'));
asserts.istrue(contains(R, 'StartFcn'));
%=============================================================================
