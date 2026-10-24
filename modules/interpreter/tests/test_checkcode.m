%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '.m'];
filewrite(filename, ['function y = nelson_checkcode_direct(x, unused)', newline(), ...
  'y = x;', newline(), ...
  'end']);
%=============================================================================
issues = checkcode(filename, '-struct', '-id');
ids = {issues.id};
asserts.istrue(any(strcmp(ids, 'NLS0007')));
%=============================================================================
txt = checkcode(filename, '-string', '-id');
asserts.istrue(contains(txt, 'NLS0007'));
%=============================================================================
rmfile(filename);
%=============================================================================
