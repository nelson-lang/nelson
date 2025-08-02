%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [-3   0; 1   0];
B = [2 ; 0];
C = [0   1];
D = [0];

sys1 = ss(A, B, C, D);
sys2 = tf(1, [8 2 1]);
sys = parallel(sys1, sys2);

A_REF = [-3.0000         0         0         0;
1.0000         0         0         0;
0         0   -0.2500   -0.2500;
0         0    0.5000         0];
B_REF = [2.0000;      0; 0.5000;       0];
C_REF = [0    1.0000         0    0.5000];
D_REF = 0;
REF = ss(A_REF, B_REF, C_REF, D_REF, 0);

assert_isapprox(sys.A, A_REF, 1e-4);
assert_isapprox(sys.B, B_REF, 1e-4);
assert_isapprox(sys.C, C_REF, 1e-4);
assert_isapprox(sys.D, D_REF, 1e-4);
assert_isapprox(sys.Ts, 0, 1e-4);
%=============================================================================
sys2 = tf(1, [8 2 1]);
sys = parallel(sys2, sys2);
assert_isapprox(sys.Numerator{1}, [0     0    16     4     2]);
assert_isapprox(sys.Denominator{1},[64    32    20     4     1]);
%=============================================================================
