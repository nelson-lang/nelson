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
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend does not provide microphone recorder objects yet.');
%=============================================================================
r = audiorecorder(8000, 8, 1);
p = getplayer(r);
asserts.isequal(p.Type, 'audioplayer');
asserts.isequal(p.TotalSamples, 0);
asserts.isfalse(isplaying(p));
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'getplayer');
asserts.checkerror('getplayer(1)', msg);
delete(r);
%=============================================================================
