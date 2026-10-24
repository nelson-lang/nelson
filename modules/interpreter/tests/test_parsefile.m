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
scriptFile = fullfile(folder, 'nelson_parsefile_script.m');
functionFile = fullfile(folder, 'nelson_parsefile_direct.m');
badFile = fullfile(folder, 'nelson_parsefile_mismatch.m');
%=============================================================================
filewrite(scriptFile, 'a = 1;');
filewrite(functionFile, sprintf(['function y = nelson_parsefile_direct(x)\n', ...
  'y = x + 1;\n', ...
  'end']));
filewrite(badFile, sprintf(['function y = nelson_parsefile_other_name(x)\n', ...
  'y = x;\n', ...
  'end']));
%=============================================================================
asserts.isequal(parsefile(scriptFile), 'script');
asserts.isequal(parsefile(functionFile), 'function');
asserts.isequal(parsefile(badFile), 'error');
%=============================================================================
rmfile(scriptFile);
rmfile(functionFile);
rmfile(badFile);
rmdir(folder);
%=============================================================================
