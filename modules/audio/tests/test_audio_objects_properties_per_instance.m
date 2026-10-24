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
%=============================================================================
% Property reads and writes reach the object they are called on, not the
% first object ever accessed.
%=============================================================================
p1 = audioplayer(zeros(100, 1), 8000);
p1.Tag = 'first';
asserts.isequal(p1.TotalSamples, 100);
p2 = audioplayer(zeros(200, 1), 16000);
p2.Tag = 'second';
asserts.isequal(p2.TotalSamples, 200);
asserts.isequal(p2.SampleRate, 16000);
asserts.isequal(p2.Tag, 'second');
asserts.isequal(p1.Tag, 'first');
asserts.isequal(p1.SampleRate, 8000);
delete(p1);
asserts.isequal(p2.Tag, 'second');
asserts.isequal(p2.TotalSamples, 200);
p3 = audioplayer(zeros(300, 1), 8000);
asserts.isequal(p3.Tag, '');
asserts.isequal(p3.TotalSamples, 300);
delete(p2);
delete(p3);
%=============================================================================
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend does not provide microphone recorder objects yet.');
%=============================================================================
r1 = audiorecorder(8000, 8, 1);
r1.Tag = 'first';
r2 = audiorecorder(22050, 16, 1);
r2.Tag = 'second';
asserts.isequal(r2.SampleRate, 22050);
asserts.isequal(r2.BitsPerSample, 16);
asserts.isequal(r2.Tag, 'second');
asserts.isequal(r1.Tag, 'first');
asserts.isequal(r1.SampleRate, 8000);
delete(r1);
asserts.isequal(r2.Tag, 'second');
delete(r2);
%=============================================================================
