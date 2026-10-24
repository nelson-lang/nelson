%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% `keyboard` is a statement keyword: code containing it must compile and run
% as long as the statement itself is not reached.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
%=============================================================================
asserts.isequal(parsestring('keyboard'), 'script');
asserts.isequal(parsestring('keyboard()'), 'error');
%=============================================================================
eval('if false, keyboard, end, evalResult = 7;');
asserts.isequal(evalResult, 7);
%=============================================================================
functionFile = fullfile(fixtureDirectory, 'keyboard_never_taken.m');
filewrite(functionFile, ['function r = keyboard_never_taken(x)', LF, ...
  '  if x > 10', LF, ...
  '    keyboard', LF, ...
  '  end', LF, ...
  '  r = 2 * x;', LF, ...
  'end', LF]);
addpath(fixtureDirectory);
asserts.isequal(keyboard_never_taken(3), 6);
rmpath(fixtureDirectory);
%=============================================================================
scriptFile = fullfile(fixtureDirectory, 'keyboard_never_taken_script.m');
filewrite(scriptFile, ['scriptResult = 5;', LF, ...
  'if scriptResult < 0', LF, ...
  '  keyboard', LF, ...
  'end', LF]);
run(scriptFile);
asserts.isequal(scriptResult, 5);
%=============================================================================
indentFile = fullfile(fixtureDirectory, 'keyboard_indent.m');
filewrite(indentFile, ['function keyboard_indent(x)', LF, ...
  'if x', LF, ...
  'keyboard', LF, ...
  'end', LF, ...
  'end']);
smartindent(indentFile, 2, false, false);
indentExpected = ['function keyboard_indent(x)', LF, ...
  '  if x', LF, ...
  '    keyboard', LF, ...
  '  end', LF, ...
  'end'];
asserts.isequal(strrep(fileread(indentFile, 'char', 'native'), char([13, 10]), LF), indentExpected);
%=============================================================================
