%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend does not provide microphone recorder objects yet.');
%=============================================================================
asserts.isequal(length(audiorecorder_used()), 0);
r1 = audiorecorder(8000, 8, 1);
asserts.isequal(length(audiorecorder_used()), 1);
r2 = audiorecorder(8000, 8, 1);
asserts.isequal(length(audiorecorder_used()), 2);
delete(r1);
asserts.isequal(length(audiorecorder_used()), 1);
delete(r2);
asserts.isequal(length(audiorecorder_used()), 0);
%=============================================================================
