%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/1206
% <-- Short Description -->
% `balance` yields wrong Transformation Matrix.
%=============================================================================
A = [1 10;0 1];
[T, B] = balance(A);
T_REF = [1, 0; 0 1];
B_REF = [1, 10;0 1];
assert_isequal(T, T_REF);
assert_isequal(B, B_REF);
assert_isequal(inv(T) * A * T == B, logical(ones(2, 2)));
%=============================================================================
