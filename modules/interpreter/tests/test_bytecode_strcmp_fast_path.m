%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% strcmp and strcmpi in a loop give the builtin results, every character
% compared, and a char overload found on the path is still called.
%=============================================================================
pairs = {{'abc', 'abc'}, {'abc', 'abd'}, {'abc', 'ab'}, {'', ''}, {'a', 'a'}, ...
  {'HeLLo', 'hello'}, {['a', char(0), 'b'], ['a', char(0), 'c']}, ...
  {['A', char(0), 'b'], ['a', char(0), 'B']}, {char(233), char(201)}, ...
  {'ab', ['a'; 'b']}, {"abc", 'abc'}, {1, 1}, {'a', 97}, {'a1_Z', 'A1_z'}};
exact = [true, false, false, true, true, false, false, false, false, false, true, false, false, false];
anyCase = [true, false, false, true, true, true, false, true, true, false, true, false, false, true];
for k = 1:numel(pairs)
  a = pairs{k}{1};
  b = pairs{k}{2};
  assert_isequal(loop_strcmp(a, b), exact(k));
  assert_isequal(loop_strcmpi(a, b), anyCase(k));
  assert_isequal(strcmp(a, b), exact(k));
  assert_isequal(strcmpi(a, b), anyCase(k));
end
assert_isequal(loop_strcmp({'a', 'b'}, 'a'), [true, false]);
%=============================================================================
% a char overload found on the path is called
d = fullfile(tempdir(), ['nelson_strcmp_', num2str(round(rand() * 1e9))]);
mkdir(fullfile(d, '@char'));
fid = fopen(fullfile(d, '@char', 'strcmp.m'), 'wt');
fprintf(fid, 'function r = strcmp(a, b)\n  r = 42;\nend\n');
fclose(fid);
assert_isequal(loop_strcmp('ab', 'ab'), true);
addpath(d);
assert_isequal(loop_strcmp('ab', 'ab'), 42);
rmpath(d);
clear functions
assert_isequal(loop_strcmp('ab', 'ab'), true);
rmdir(d, 's');
%=============================================================================
function r = loop_strcmp(a, b)
  r = [];
  for k = 1:2
    r = strcmp(a, b);
  end
end
%=============================================================================
function r = loop_strcmpi(a, b)
  r = [];
  for k = 1:2
    r = strcmpi(a, b);
  end
end
%=============================================================================
