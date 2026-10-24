%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_table_display_nd_size()
  % a row with more than 2 dimensions shows its size and class, the size
  % written with the times sign as in the cell display and the table header
  x = char(215);
  T = table(ones(2, 2, 2));
  expected = string(['1', x, '2', x, '2 double']);
  assert_isequal(display_lines(T), [expected; expected]);
  %=============================================================================
  T = table(repmat("s", 2, 1, 2));
  expected = string(['1', x, '1', x, '2 string']);
  assert_isequal(display_lines(T), [expected; expected]);
  %=============================================================================
  T = table(categorical(repmat({'a'}, 1, 1, 2)));
  assert_isequal(display_lines(T), string(['1', x, '1', x, '2 categorical']));
  %=============================================================================
  T = table(repmat({'a'}, 1, 1, 2));
  assert_isequal(display_lines(T), string(['1', x, '1', x, '2 cell']));
  %=============================================================================
  % same text as the cell display of the same value
  T = table({ones(2, 2, 2)});
  assert_isequal(display_lines(T), string(['{2', x, '2', x, '2 double}']));
end
%=============================================================================
function L = display_lines(T)
  % the data rows of disp(T), without the header and the rule
  L = strtrim(splitlines(string(evalc('disp(T)'))));
  L(L == "") = [];
  L = L(3:end);
end
%=============================================================================
