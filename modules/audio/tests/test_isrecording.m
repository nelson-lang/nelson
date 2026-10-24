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
asserts.isfalse(isrecording(r));
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'isrecording');
asserts.checkerror('isrecording(1)', msg);
delete(r);
%=============================================================================
