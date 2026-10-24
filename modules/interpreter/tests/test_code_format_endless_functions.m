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
% The closing 'end' of a function is optional: its body then runs up to the
% next 'function' or the end of the file. smartindent and nelson-format must
% accept such files, alone or mixed with functions closed by 'end'.
workdir = tempname();
mkdir(workdir);
workdirCleanup = onCleanup(@() rmdir(workdir, 's')); %#ok<NASGU>
addpath(workdir);
pathCleanup = onCleanup(@() rmpath(workdir)); %#ok<NASGU>
formatCommand = nelson_tool_command_for_test('nelson-format');
nl = char(10);
%=============================================================================
endlessExpected = ['function r = endless_format_case(a)', nl, ...
  '  r = helper(a);', nl, ...
  '  if r > 0', nl, ...
  '    r = r + 1;', nl, ...
  '  end', nl, ...
  nl, ...
  'function r = helper(a)', nl, ...
  '  r = 10 * a;', nl];
mixedExpected = ['function r = mixed_format_case(a)', nl, ...
  '  r = first(a) + second(a);', nl, ...
  'end', nl, ...
  'function r = first(a)', nl, ...
  '  r = a;', nl, ...
  '  % still in the body of first', nl, ...
  'function r = second(a)', nl, ...
  '  r = 10 * a;', nl, ...
  'end', nl];
cases = {'endless_format_case', endlessExpected; 'mixed_format_case', mixedExpected};
%=============================================================================
for k = 1:size(cases, 1)
  fixtureFile = [workdir, '/', cases{k, 1}, '.m'];
  expected = cases{k, 2};
  unindented = regexprep(expected, '(^|\n) +', '$1');

  filewrite(fixtureFile, unindented);
  smartindent(fixtureFile, 2, false, true);
  formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
  asserts.isequal(formatted, expected);

  filewrite(fixtureFile, unindented);
  [status, output] = system([formatCommand, ' "', fixtureFile, '"']);
  asserts.isequal(status, 0);
  asserts.istrue(contains(output, [cases{k, 1}, '.m']));
  formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
  asserts.isequal(formatted, expected);
end
rehash();
asserts.isequal(endless_format_case(1), 11);
asserts.isequal(mixed_format_case(1), 11);
%=============================================================================
% An unclosed 'if' is still reported, even in a function without 'end'.
unclosedFile = [workdir, '/unclosed_if_case.m'];
unclosedInput = ['function r = unclosed_if_case(a)', nl, 'if a > 0', nl, 'r = 1;', nl];
filewrite(unclosedFile, unclosedInput);
asserts.checkerror('smartindent(unclosedFile, 2, false, true)', _('Missing ''end'' in code passed to smartindent.'));
asserts.isequal(strrep(fileread(unclosedFile, 'char', 'native'), char([13, 10]), nl), unclosedInput);
%=============================================================================
