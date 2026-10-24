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
asserts.checkerror('playblocking(p, 0)', _('start >= 1 expected.'));
asserts.checkerror('playblocking(p, [2 1])', _('Invalid range.'));
asserts.checkerror('playblocking(p, [1 12])', _('Invalid range.'));
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'playblocking');
if ~iswasm()
  asserts.checkerror('playblocking(1)', ...
    msg);
end
delete(p);
%=============================================================================
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend does not provide microphone recorder objects yet.');
%=============================================================================
r = audiorecorder(8000, 8, 1);
asserts.checkerror('playblocking(r, 0)', _('start >= 1 expected.'));
asserts.checkerror('playblocking(r, [1 2])', _('Invalid range.'));
delete(r);
%=============================================================================
