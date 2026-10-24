%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a calendarDuration array is a table variable with one row per element
T = table(["a"; "b"], calmonths([1; 2]));
assert_isequal(size(T), [2 2]);
assert_isequal(class(T.Var2), 'calendarDuration');
assert_isequal(size(T.Var2), [2 1]);
%=============================================================================
% assigning a row keeps a calendarDuration column
T(2, :) = {"r", calyears(1)};
assert_isequal(size(T), [2 2]);
assert_isequal(T.Var1, ["a"; "r"]);
assert_isequal(class(T.Var2), 'calendarDuration');
assert_isequal(size(T.Var2), [2 1]);
assert_isequal(calmonths(T.Var2), [1; 12]);
%=============================================================================
% assigning a new row grows the calendarDuration column
T(3, :) = {"s", caldays(3)};
assert_isequal(size(T), [3 2]);
assert_isequal(size(T.Var2), [3 1]);
assert_isequal(split(T.Var2, 'days'), [0; 0; 3]);
%=============================================================================
