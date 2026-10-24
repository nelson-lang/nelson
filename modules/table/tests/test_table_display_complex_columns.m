%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_table_display_complex_columns()
  % a complex variable with several columns shows its values, not its size
  T = table([1+2i, 3+4i; 5+6i, 7-8i]);
  assert_isequal(display_lines(T), ["1+2i    3+4i"; "5+6i    7-8i"]);
  %=============================================================================
  T = table([1.5+2i, 3+4.25i, 5+6i]);
  assert_isequal(display_lines(T), "1.5+2i         3+4.25i      5+6i");
  %=============================================================================
  T = table([1+2i, 3+4i, 5+6i, 7+8i, 9+10i]);
  assert_isequal(display_lines(T), "1+2i     3+4i     5+6i     7+8i     9+10i");
  %=============================================================================
  % one column: unchanged
  T = table([1+2i; 3-4i]);
  assert_isequal(display_lines(T), ["1+2i"; "3-4i"]);
end
%=============================================================================
function L = display_lines(T)
  % the data rows of disp(T), without the header and the rule
  L = strtrim(splitlines(string(evalc('disp(T)'))));
  L(L == "") = [];
  L = L(3:end);
end
%=============================================================================
