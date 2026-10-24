%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
% <--WEIGHT 8-->
% A function with a nested function is only accepted by a fallback parse: its
% formatting tokens must still line up with the original source, both for
% smartindent (evaluator) and nelson-format (no evaluator).
workdir = tempname();
mkdir(workdir);
workdirCleanup = onCleanup(@() rmdir(workdir, 's')); %#ok<NASGU>
nl = char(10);
expected = ['function [parentAfter, innerResult] = nested_format_case()', nl, ...
  '  f = 1;', nl, ...
  '  innerResult = inner(42);', nl, ...
  '  parentAfter = f;', nl, ...
  '  function r = inner(f)', nl, ...
  '    % f is local to the nested function.', nl, ...
  '    if f > 0', nl, ...
  '      r = f + 1;', nl, ...
  '    else', nl, ...
  '      r = 0;', nl, ...
  '    end', nl, ...
  '  end', nl, ...
  'end', nl];
unindented = regexprep(expected, '(^|\n) +', '$1');
fixtureFile = [workdir, '/nested_format_case.m'];
%=============================================================================
filewrite(fixtureFile, unindented);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
formatCommand = nelson_tool_command_for_test('nelson-format');
filewrite(fixtureFile, unindented);
[status, output] = system([formatCommand, ' "', fixtureFile, '"']);
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'nested_format_case.m'));
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
[status, output] = system([formatCommand, ' --check "', fixtureFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');
%=============================================================================
