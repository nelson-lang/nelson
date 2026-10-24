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
p = audioplayer(zeros(10, 1), 8000);
resume(p);
asserts.isfalse(isplaying(p));
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'resume');
asserts.checkerror('resume(1)', ...
  msg);
delete(p);
%=============================================================================
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend does not provide microphone recorder objects yet.');
%=============================================================================
r = audiorecorder(8000, 8, 1);
asserts.checkerror('resume(r, 1)', _('Too many input arguments.'));
delete(r);
%=============================================================================
