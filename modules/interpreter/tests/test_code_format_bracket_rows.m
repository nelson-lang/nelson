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
% The rows of a bracket literal spanning several lines without "..." are
% continuation lines: the author's extra indentation is kept, one level is
% given when there is none, and the line closing the outermost bracket goes
% back to the statement indentation.
workdir = tempname();
mkdir(workdir);
workdirCleanup = onCleanup(@() rmdir(workdir, 's')); %#ok<NASGU>
nl = char(10);
expected = ['function r = br_case()', nl, ...
  '  requests = {', nl, ...
  '    ''{"id":1}'';', nl, ...
  '    % second request', nl, ...
  '    ''{"id":2}''', nl, ...
  '  };', nl, ...
  '  m = [1, 2', nl, ...
  '    3, 4];', nl, ...
  '  if ~isempty(m)', nl, ...
  '    c = {', nl, ...
  '      {1, 2}', nl, ...
  '      {3, 4}', nl, ...
  '    };', nl, ...
  '  end', nl, ...
  '  r = {requests, m, c};', nl, ...
  'end', nl];
fixtureFile = [workdir, '/br_case.m'];
%=============================================================================
% Already formatted: unchanged by smartindent.
filewrite(fixtureFile, expected);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
% Unindented: rows one level more than their statement, closer at its level.
unindented = regexprep(expected, '(^|\n) +', '$1');
filewrite(fixtureFile, unindented);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
% The author's extra row indentation is kept.
aligned = strrep(expected, '    3, 4];', '       3, 4];');
filewrite(fixtureFile, aligned);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, aligned);
%=============================================================================
% Indentation only (no full format).
filewrite(fixtureFile, unindented);
smartindent(fixtureFile, 2, false, false);
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
