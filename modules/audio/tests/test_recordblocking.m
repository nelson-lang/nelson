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
r = audiorecorder(8000, 8, 1);
asserts.checkerror('recordblocking(r, 0)', _('duration > 0 expected.'));
asserts.checkerror('recordblocking(r, -1)', _('duration > 0 expected.'));
asserts.checkerror('recordblocking(r, ''abc'')', _('duration in seconds expected.'));
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'recordblocking');
asserts.checkerror('recordblocking(1, 1)', ...
  msg);
delete(r);
%=============================================================================
