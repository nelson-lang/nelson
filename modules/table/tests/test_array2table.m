%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [1 4 7; 2 5 8; 3 6 9];
T = array2table(A);
assert_istrue(istable(T));
assert_isequal(size(T), [3, 3]);
assert_isequal(T.A1, [1; 2; 3]);
assert_isequal(T.A2, [4; 5; 6]);
assert_isequal(T.A3, [7; 8; 9]);
%=============================================================================
T = array2table([1 4 7; 2 5 8; 3 6 9]);
assert_istrue(istable(T));
assert_isequal(size(T), [3, 3]);
assert_isequal(T.Var1, [1; 2; 3]);
assert_isequal(T.Var2, [4; 5; 6]);
assert_isequal(T.Var3, [7; 8; 9]);
%=============================================================================
