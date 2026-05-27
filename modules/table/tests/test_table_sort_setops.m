%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([2; 1; 2], {'b'; 'a'; 'b'}, 'VariableNames', {'A', 'B'});
S = sortrows(T, 'A');
assert_isequal(S.A, [1; 2; 2]);
[U, ia] = unique(T);
assert_isequal(height(U), 2);
A = table([1; 2; 3], 'VariableNames', {'K'});
B = table([2; 3; 4], 'VariableNames', {'K'});
[tf, loc] = ismember(A, B);
assert_isequal(tf, [false; true; true]);
assert_isequal(loc, [0; 1; 2]);
I = intersect(A, B);
assert_isequal(I.K, [2; 3]);
D = setdiff(A, B);
assert_isequal(D.K, 1);
U = union(A, B);
assert_isequal(U.K, [1; 2; 3; 4]);
X = setxor(A, B);
assert_isequal(X.K, [1; 4]);
%=============================================================================
