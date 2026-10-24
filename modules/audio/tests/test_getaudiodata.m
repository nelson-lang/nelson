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
R = getaudiodata(r);
asserts.isequal(R, []);
R = getaudiodata(r, 'single');
asserts.isequal(R, []);
asserts.checkerror('getaudiodata(r, ''int32'')', ...
  _('Unsupported data type for audiorecorder getaudiodata.'));
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'getaudiodata');
asserts.checkerror('getaudiodata(1)', ...
  msg);
delete(r);
%=============================================================================
