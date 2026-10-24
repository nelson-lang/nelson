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
% nelson-format parses without a workspace: command syntax ("name -opt arg")
% must follow the same rule as the interpreter, with the variables known from
% the source itself, and command arguments must be kept as written.
workdir = tempname();
mkdir(workdir);
workdirCleanup = onCleanup(@() rmdir(workdir, 's')); %#ok<NASGU>
formatCommand = nelson_tool_command_for_test('nelson-format');
nl = char(10);
input = ['csFmtA = 6;', nl, ...
  'csFmtB = 2;', nl, ...
  'cs_fmt_probe -x data.mat', nl, ...
  'cs_fmt_probe -a b', nl, ...
  'cs_fmt_probe /tmp/foo', nl, ...
  'cs_fmt_probe +tag', nl, ...
  'csFmtC = csFmtA -csFmtB;', nl, ...
  'csFmtA -csFmtB', nl];
expected = ['csFmtA = 6;', nl, ...
  'csFmtB = 2;', nl, ...
  'cs_fmt_probe -x data.mat', nl, ...
  'cs_fmt_probe -a b', nl, ...
  'cs_fmt_probe /tmp/foo', nl, ...
  'cs_fmt_probe +tag', nl, ...
  'csFmtC = csFmtA - csFmtB;', nl, ...
  'csFmtA -csFmtB', nl];
fixtureFile = [workdir, '/command_syntax_case.m'];
%=============================================================================
filewrite(fixtureFile, input);
[status, output] = system([formatCommand, ' "', fixtureFile, '"']);
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'command_syntax_case.m'));
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
filewrite(fixtureFile, input);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
