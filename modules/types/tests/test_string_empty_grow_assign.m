%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Growing an empty string array fills the new elements with <missing>.
%=============================================================================
values = {3, true, int8(3), 2.7, 'x', "5", single(2)};
expected = ["3", "true", "3", "2.7", "x", "5", "2"];
for k = 1:numel(values)
  c = string([]);
  c(2) = values{k};
  assert_isequal(ismissing(c), [true false]);
  assert_isequal(c(2), expected(k));
  c = string([]);
  c(1, 2) = values{k};
  assert_isequal(ismissing(c), [true false]);
  assert_isequal(c(2), expected(k));
end
%=============================================================================
c = string([]);
c(2) = 3i;
assert_isequal(ismissing(c), [true false]);
assert_isequal(c(2), "0+3i");
%=============================================================================
c = string([]);
c(1:2) = [3i 3i];
assert_isequal(c, ["0+3i", "0+3i"]);
%=============================================================================
c = string([]);
c(1, 1:2) = [3i, 1 - 2i];
assert_isequal(c, ["0+3i", "1-2i"]);
%=============================================================================
c = string([]);
c(2, 3) = "a";
assert_isequal(size(c), [2 3]);
assert_isequal(ismissing(c), [true true true; true true false]);
assert_isequal(c(2, 3), "a");
%=============================================================================
c = string([]);
c(2) = 3;
c(4) = 5;
assert_isequal(ismissing(c), [true false true false]);
%=============================================================================
c = "a";
c(3) = "b";
assert_isequal(ismissing(c), [false true false]);
%=============================================================================
% cellstr turns <missing> into empty text
c = string([]);
c(3) = "z";
assert_isequal(cellstr(c), {'', '', 'z'});
%=============================================================================
assert_isequal(string([1i, 2 - 3i, 4]), ["0+1i", "2-3i", "4+0i"]);
%=============================================================================
