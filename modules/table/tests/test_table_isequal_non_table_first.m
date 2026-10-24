%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% isequal is false when the first argument is not a table or timetable.
%=============================================================================
A = table([1; 2]);
assert_isfalse(isequal(5, A));
assert_isfalse(isequal(A, 5));
assert_isfalse(isequal([], A));
assert_isfalse(isequal({}, A));
assert_isfalse(isequal('A', A));
assert_isfalse(isequal(5, A, A));
assert_istrue(isequal(A, A));
assert_istrue(isequal(A, A, A));
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2]);
assert_isfalse(isequal(5, TT));
assert_isfalse(isequal(TT, 5));
assert_isfalse(isequal([], TT));
assert_isfalse(isequal(5, TT, TT));
assert_isfalse(isequal(A, TT));
assert_isfalse(isequal(TT, A));
assert_istrue(isequal(TT, TT));
%=============================================================================
E = eventtable(seconds(1));
assert_isfalse(isequal(5, E));
assert_isfalse(isequal(A, E));
%=============================================================================
