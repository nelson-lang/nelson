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
% A comment line after a "..." continuation is written once, at the
% indentation of the continuation lines.
workdir = tempname();
mkdir(workdir);
workdirCleanup = onCleanup(@() rmdir(workdir, 's')); %#ok<NASGU>
nl = char(10);
expected = ['function r = cc_case()', nl, ...
  '  c = {''a'', ...', nl, ...
  '    % first note', nl, ...
  '    % second note', nl, ...
  '    ''b''};', nl, ...
  '  m = [1, 2, ... % after the dots', nl, ...
  '    % third note', nl, ...
  '    3];', nl, ...
  '  r = {c, m};', nl, ...
  'end', nl];
fixtureFile = [workdir, '/cc_case.m'];
%=============================================================================
% Already formatted: unchanged by smartindent.
filewrite(fixtureFile, expected);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
% Unindented: the comment lines move with the continuation, no copy.
unindented = regexprep(expected, '(^|\n) +', '$1');
filewrite(fixtureFile, unindented);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
formatCommand = nelson_tool_command_for_test('nelson-format');
filewrite(fixtureFile, expected);
[status, output] = system([formatCommand, ' --check "', fixtureFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');
filewrite(fixtureFile, unindented);
[status, output] = system([formatCommand, ' "', fixtureFile, '"']);
asserts.isequal(status, 0);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
