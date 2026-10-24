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
asserts.checkerror('play(p, 0)', _('start >= 1 expected.'));
asserts.checkerror('play(p, [2 1])', _('Invalid range.'));
asserts.checkerror('play(p, [1 12])', _('Invalid range.'));
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'play');
if ~iswasm()
  asserts.checkerror('play(1)', ...
    msg);
end
delete(p);
%=============================================================================
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend does not provide microphone recorder objects yet.');
%=============================================================================
r = audiorecorder(8000, 8, 1);
asserts.checkerror('play(r, 0)', _('start >= 1 expected.'));
asserts.checkerror('play(r, [1 2])', _('Invalid range.'));
delete(r);
%=============================================================================
