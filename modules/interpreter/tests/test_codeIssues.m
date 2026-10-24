%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
folder = tempname();
mkdir(folder);
filename = fullfile(folder, 'nelson_codeIssues_direct.m');
filewrite(filename, ['function y = nelson_codeIssues_direct(x, unused)', newline(), ...
  'y = x;', newline(), ...
  'end']);
%=============================================================================
ci = codeIssues(folder);
asserts.isequal(class(ci), 'codeIssues');
asserts.isequal(ci.Files, {folder});
asserts.istrue(istable(ci.Issues));
asserts.istrue(height(ci.Issues) >= 1);
asserts.istrue(any(strcmp(ci.Issues.CheckID, 'NLS0007')));
%=============================================================================
rmfile(filename);
rmdir(folder);
%=============================================================================
