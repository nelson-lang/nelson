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
% A function handle keeps "@" next to its name ("@sin", not "@ sin") and a
% colon index keeps the space after a separator ("h(end, :)", "h(1, :)").
workdir = tempname();
mkdir(workdir);
workdirCleanup = onCleanup(@() rmdir(workdir, 's')); %#ok<NASGU>
nl = char(10);
expected = ['function s = hc_case(h, c)', nl, ...
  '  d = repmat(@sin, 0, 3);', nl, ...
  '  g = {@sin, @cos};', nl, ...
  '  f = @(x) x + 1;', nl, ...
  '  s = h(end, :);', nl, ...
  '  t = h(1, :);', nl, ...
  '  u = h(:, end);', nl, ...
  '  v = h(1:end, :, 2);', nl, ...
  '  w = c{end, :};', nl, ...
  '  m = [h(end, :), ''a''];', nl, ...
  'end', nl];
fixtureFile = [workdir, '/hc_case.m'];
%=============================================================================
% Already formatted: unchanged by smartindent.
filewrite(fixtureFile, expected);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
% Unspaced: separators get their space back, also before ":".
unspaced = strrep(expected, ', ', ',');
filewrite(fixtureFile, unspaced);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
formatCommand = nelson_tool_command_for_test('nelson-format');
filewrite(fixtureFile, expected);
[status, output] = system([formatCommand, ' --check "', fixtureFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');
filewrite(fixtureFile, unspaced);
[status, output] = system([formatCommand, ' "', fixtureFile, '"']);
asserts.isequal(status, 0);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
