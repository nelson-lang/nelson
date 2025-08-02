%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('slicot_ab04md'), 7);
assert_isequal(nargout('slicot_ab04md'), 5);
%=============================================================================
N = 2;
M = 2;
P = 2;
TYPE= 'C';
ALPHA = 1;
BETA = 1;
A = [   1.0  0.5; 0.5  1.0];
B = [   0.0 -1.0; 1.0  0.0];
C = [  -1.0  0.0; 0.0  1.0];
D = [   1.0  0.0; 0.0 -1.0];
[A_OUT, B_OUT, C_OUT, D_OUT, INFO] = slicot_ab04md(TYPE, ALPHA, BETA, A, B, C, D);
A_OUT_REF = [-1   -4;   -4   -1];
B_OUT_REF = [-2.8284     -0.0000; -0.0000      2.8284];
C_OUT_REF = [-0.0000      2.8284; -2.8284      0.0000];
D_OUT_REF = [3     0; 0     1]; % blas dependency
INFO_REF = int32(0);
assert_isequal(A_OUT, A_OUT_REF);
assert_isapprox(B_OUT, B_OUT_REF, 1e-4);
assert_isapprox(C_OUT, C_OUT_REF, 1e-4);
assert_isequal(D_OUT, D_OUT_REF);
assert_isequal(INFO, INFO_REF);
