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
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend does not provide microphone recorder objects yet.');
%=============================================================================
r = audiorecorder();
recordblocking(r, 5);
asserts.istrue(isvalid(r));
delete(r)
asserts.isfalse(isvalid(r));
%=============================================================================
