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
% A separator keeps its space before an opening bracket ("f(a, [1 2])",
% "[r; [1, 2]]") and a "..." continuation line keeps an extra indentation:
% the author's one when there is one, one indentation level otherwise.
workdir = tempname();
mkdir(workdir);
workdirCleanup = onCleanup(@() rmdir(workdir, 's')); %#ok<NASGU>
nl = char(10);
expected = ['function h = sp_case(ax)', nl, ...
  '  h = line(ax, [0 1], [0 1]);', nl, ...
  '  r = {};', nl, ...
  '  r = [r; [1, 2]];', nl, ...
  '  c = {r, {1, 2}};', nl, ...
  '  m = replace(ax, [char(10)], '''');', nl, ...
  '  plot(ax, ...', nl, ...
  '      r{:}, m{:});', nl, ...
  '  if ~isempty(r)', nl, ...
  '    x = [1, ...', nl, ...
  '      -2];', nl, ...
  '  end', nl, ...
  'end', nl];
fixtureFile = [workdir, '/sp_case.m'];
%=============================================================================
% Already formatted: unchanged by smartindent.
filewrite(fixtureFile, expected);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
% Unindented: block lines move to their level, continuation lines one level more.
unindented = regexprep(expected, '(^|\n) +', '$1');
expectedFromFlat = strrep(expected, '      r{:}, m{:});', '    r{:}, m{:});');
filewrite(fixtureFile, unindented);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expectedFromFlat);
%=============================================================================
% Unspaced: separators get their space back, also before "[" and "{".
unspaced = strrep(strrep(expected, ', ', ','), '; ', ';');
filewrite(fixtureFile, unspaced);
smartindent(fixtureFile, 2, false, true);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expected);
%=============================================================================
% Indentation only (no full format): continuation indentation kept too.
filewrite(fixtureFile, unindented);
smartindent(fixtureFile, 2, false, false);
formatted = strrep(fileread(fixtureFile, 'char', 'native'), char([13, 10]), nl);
asserts.isequal(formatted, expectedFromFlat);
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
