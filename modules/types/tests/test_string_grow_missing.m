%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% the elements added when a string array grows are <missing>
%=============================================================================
s = "a";
s(3) = "c";
assert_isequal(ismissing(s), [false true false]);
assert_isequal(s([1 3]), ["a" "c"]);
%=============================================================================
s = strings(0);
s(2) = "x";
assert_isequal(ismissing(s), [true false]);
%=============================================================================
s = strings(0);
s(1) = "x";
assert_isequal(s, "x");
%=============================================================================
s = strings(0);
s(end + 1) = "x";
assert_isequal(s, "x");
%=============================================================================
s = "a";
s(end + 1) = "x";
assert_isequal(s, ["a" "x"]);
%=============================================================================
s = "a";
s(2, 2) = "b";
assert_isequal(ismissing(s), [false true; true false]);
%=============================================================================
s = "a";
s(2, 2, 2) = "b";
assert_isequal(size(s), [2 2 2]);
assert_isequal(nnz(ismissing(s)), 6);
%=============================================================================
s = strings(1, 0);
s(3) = "x";
assert_isequal(ismissing(s), [true true false]);
%=============================================================================
s = strings(2, 0);
s(2, 2) = "b";
assert_isequal(ismissing(s), [true true; true false]);
%=============================================================================
s = ["a"; "b"];
s(4) = "d";
assert_isequal(ismissing(s), [false; false; true; false]);
%=============================================================================
s = strings(1, 2);
s(4) = "q";
assert_isequal(ismissing(s), [false false true false]);
%=============================================================================
% delete then grow
s = ["a" "b" "c"];
s(2) = [];
s(4) = "z";
assert_isequal(ismissing(s), [false false true false]);
%=============================================================================
s = "a";
s(:, 3) = "z";
assert_isequal(ismissing(s), [false true false]);
%=============================================================================
s = "a";
s(1:3) = "q";
assert_isequal(s, ["q" "q" "q"]);
%=============================================================================
s = "a";
s(3) = 5;
assert_isequal(ismissing(s), [false true false]);
%=============================================================================
s = "a";
s(5) = missing;
assert_isequal(ismissing(s), [false true true true true]);
%=============================================================================
s = "a";
s{3} = 'c';
assert_isequal(ismissing(s), [false true false]);
%=============================================================================
% repeated growth reuses spare capacity
s = strings(0);
for k = 1:40
  s(2 * k) = "x";
end
assert_isequal(ismissing(s), repmat([true false], 1, 40));
%=============================================================================
% arrays built without growing hold empty text
assert_isequal(strings(2, 2), ["" ""; "" ""]);
assert_isfalse(any(ismissing(strings(3)), 'all'));
assert_isequal(size(string.empty), [0 0]);
assert_isequal(repmat("a", 2, 2), ["a" "a"; "a" "a"]);
assert_isequal([string.empty "a"], "a");
assert_isequal(cat(1, strings(0, 1), "a"), "a");
assert_isequal(string({'a', ''; 'b', 'c'}), ["a" ""; "b" "c"]);
%=============================================================================
