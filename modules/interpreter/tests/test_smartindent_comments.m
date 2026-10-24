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
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
fixtureFile = fullfile(fixtureDirectory, 'quoted_comments.m');
sq = char(39);
dq = char(34);
nl = char(10);
source = ['a = ', sq, '%{', sq, ';', nl, ...
  'b = ', dq, '%{', dq, ';', nl, ...
  'c = ', sq, 'a', sq, sq, '%{', sq, ';', nl, ...
  'd = ', dq, 'a', dq, dq, '%{', dq, ';', nl, ...
  'e = ', sq, '%}', sq, ';', nl, ...
  'f = {', sq, '%{', sq, ' ', sq, '%}', sq, '};', nl, ...
  'g = [1 2]', sq, ';', nl, ...
  'h = g.', sq, ';', nl, ...
  'v_ = complex([1 3], [2 -4]);', nl, ...
  'underscoreTranspose = v_.', sq, ';', nl, ...
  'underscoreConjugate = v_', sq, ';', nl, ...
  'underscoreDouble = v_', sq, sq, ';', nl, ...
  '% A line comment containing ', sq, '%{', sq, ' is not a block.', nl, ...
  '%{', nl, ...
  'unmatched quotes ', sq, ' ', dq, ' and %{ remain comment text', nl, ...
  '%}', nl, ...
  'value = 3;'];
filewrite(fixtureFile, source);
smartindent(fixtureFile, 2, false, false);
asserts.isequal(strrep(fileread(fixtureFile, 'char', 'native'), char([13 10]), nl), source);
smartindent(fixtureFile, 2, false, true);
asserts.isequal(strrep(fileread(fixtureFile, 'char', 'native'), char([13 10]), nl), source);
smartindent(fixtureFile, 2, false, true);
asserts.isequal(strrep(fileread(fixtureFile, 'char', 'native'), char([13 10]), nl), source);
run(fixtureFile);
asserts.isequal(a, char([37 123]));
asserts.isequal(b, string(a));
asserts.isequal(c, ['a', sq, a]);
asserts.isequal(d, string(['a', dq, a]));
asserts.isequal(e, char([37 125]));
asserts.isequal(f, {a, e});
asserts.isequal(g, [1; 2]);
asserts.isequal(h, [1 2]);
asserts.isequal(underscoreTranspose, complex([1; 3], [2; -4]));
asserts.isequal(underscoreConjugate, complex([1; 3], [-2; 4]));
asserts.isequal(underscoreDouble, v_);
asserts.isequal(value, 3);

% The CLI shares the same mask and must preserve quoted marker bytes.
formatCommand = nelson_tool_command_for_test('nelson-format');
[status, output] = system([formatCommand, ' --check "', fixtureFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');
minimalSource = ['x=', sq, '%{', sq, ';'];
filewrite(fixtureFile, minimalSource);
smartindent(fixtureFile, 2, false, true);
asserts.isequal(fileread(fixtureFile, 'char', 'native'), ['x = ', sq, '%{', sq, ';']);

% A real unterminated block still fails without writing a backup or output.
invalidSource = [minimalSource, nl, '%{', nl, 'unterminated comment'];
filewrite(fixtureFile, invalidSource);
invalidOriginal = fileread(fixtureFile, 'char', 'native');
didError = false;
try
  smartindent(fixtureFile, 2, true, true);
catch ex
  didError = contains(getReport(ex), 'Unterminated block comment');
end
asserts.istrue(didError);
asserts.isequal(fileread(fixtureFile, 'char', 'native'), invalidOriginal);
asserts.isfalse(isfile([fixtureFile, '.bak']));
%=============================================================================
